(*
 * Copyright (c) 2016-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Type_core
module Sizer = Size.Sizer
module Bin = Binary

let rec t : type a. a t -> a Sizer.t = function
  | Self s -> fst (self s)
  | Custom c -> c.size_of
  | Map b -> map ~boxed:true b
  | Prim t -> prim ~boxed:true t
  | Attributes { attr_type; _ } -> t attr_type
  | Boxed b -> t b
  | List l -> Bin.List.sizer l.len (t l.v)
  | Array a -> Bin.Array.sizer a.len (t a.v)
  | Tuple t -> tuple t
  | Option x -> Bin.Option.sizer (t x)
  | Record r -> record r
  | Variant v -> variant v
  | Var v -> raise (Unbound_type_variable v)

and unboxed : type a. a t -> a Sizer.t = function
  | Self s -> snd (self s)
  | Custom c -> c.unboxed_size_of
  | Map b -> map ~boxed:false b
  | Prim t -> prim ~boxed:false t
  | Attributes { attr_type = t; _ } -> unboxed t
  | Boxed b -> t b
  | List l -> Bin.List.sizer l.len (t l.v)
  | Array a -> Bin.Array.sizer a.len (t a.v)
  | Tuple t -> tuple t
  | Option x -> Bin.Option.sizer (t x)
  | Record r -> record r
  | Variant v -> variant v
  | Var v -> raise (Unbound_type_variable v)

and self : type a. a self -> a Sizer.t * a Sizer.t =
  (* The resulting sizer may be any of [Unknown], [Static] or [Dynamic]. In the
     latter case, we must be able to recurse back to this definition at size
     computation time.

     We unroll with 'stub' dynamic values that initially [assert false] but will
     be backpatched with the parent derivation (iff it does actually turn out to
     be dynamic). *)
  let stub _ = assert false in
  let backpatch stubref = function
    | Size.Dynamic f -> stubref := f
    | Size.Static _ -> ()
    | Size.Unknown -> ()
  in
  fun { self_unroll; _ } ->
    let of_value = ref stub
    and of_encoding = ref stub
    and unboxed_of_value = ref stub
    and unboxed_of_encoding = ref stub in
    let unrolled =
      let size_of =
        Sizer.dynamic
          ~of_value:(fun a -> !of_value a)
          ~of_encoding:(fun buf off -> !of_encoding buf off)
      in
      let unboxed_size_of =
        Sizer.dynamic
          ~of_value:(fun a -> !unboxed_of_value a)
          ~of_encoding:(fun buf off -> !unboxed_of_encoding buf off)
      in
      self_unroll (partial ~size_of ~unboxed_size_of ())
    in
    let t = t unrolled and unboxed = unboxed unrolled in
    backpatch of_value t.of_value;
    backpatch of_encoding t.of_encoding;
    backpatch unboxed_of_value unboxed.of_value;
    backpatch unboxed_of_encoding unboxed.of_encoding;
    (t, unboxed)

and tuple : type a. a tuple -> a Sizer.t = function
  | Pair (x, y) -> Bin.Pair.sizer (t x) (t y)
  | Triple (x, y, z) -> Bin.Triple.sizer (t x) (t y) (t z)

and map : type a b. boxed:bool -> (a, b) map -> b Sizer.t =
 fun ~boxed { x; g; _ } -> Sizer.using g (if boxed then t x else unboxed x)

and prim : type a. boxed:bool -> a prim -> a Sizer.t =
 fun ~boxed -> function
  | Unit -> Bin.Unit.sizer
  | Bool -> Bin.Bool.sizer
  | Char -> Bin.Char.sizer
  | Int -> Bin.Varint.sizer
  | Int32 -> Bin.Int32.sizer
  | Int64 -> Bin.Int64.sizer
  | Float -> Bin.Float.sizer
  | String n -> (if boxed then Bin.String.sizer else Bin.String_unboxed.sizer) n
  | Bytes n -> (if boxed then Bin.Bytes.sizer else Bin.Bytes_unboxed.sizer) n

and record : type a. a record -> a Sizer.t =
 fun r ->
  fields r
  |> List.map (fun (Field f) -> Sizer.using f.fget (t f.ftype))
  |> ListLabels.fold_left ~init:(Sizer.static 0) ~f:Sizer.( <+> )

and variant : type a. a variant -> a Sizer.t =
 fun v ->
  let static_varint_size n =
    match Bin.Varint.sizer.of_value with
    | Unknown | Static _ -> assert false
    | Dynamic f -> f n
  in
  let case_lengths : (int * a Sizer.t) array =
    ArrayLabels.map v.vcases ~f:(function
      | C0 { ctag0; _ } -> (static_varint_size ctag0, Sizer.static 0)
      | C1 { ctag1; ctype1; cwit1 = expected; _ } ->
          let tag_length = static_varint_size ctag1 in
          let arg_length =
            match t ctype1 with
            | ({ of_value = Static _; _ } | { of_value = Unknown; _ }) as t -> t
            | { of_value = Dynamic of_value; of_encoding } ->
                let of_value a =
                  match v.vget a with
                  | CV0 _ -> assert false
                  | CV1 ({ cwit1 = received; _ }, args) -> (
                      match Witness.cast received expected args with
                      | Some v -> of_value v
                      | None -> assert false)
                in
                { of_value = Dynamic of_value; of_encoding }
          in
          (tag_length, arg_length))
  in
  (* If all cases have [size = Static n], then so does the variant.
     If any case has [size = Unknown], then so does the variant. *)
  let non_dynamic_length =
    let rec aux static_so_far = function
      | -1 -> Option.map (fun n -> Sizer.static n) static_so_far
      | i -> (
          match case_lengths.(i) with
          | _, { of_value = Unknown; _ } -> Some Sizer.unknown
          | _, { of_value = Dynamic _; _ } -> None
          | tag_len, { of_value = Static arg_len; _ } -> (
              let len = tag_len + arg_len in
              match static_so_far with
              | None -> aux (Some len) (i - 1)
              | Some len' when len = len' -> aux static_so_far (i - 1)
              | Some _ -> None))
    in
    aux None (Array.length case_lengths - 1)
  in
  match non_dynamic_length with
  | Some x -> x
  | None ->
      (* Otherwise, the variant size is [Dynamic] over the tag *)
      let of_value a =
        let tag =
          match v.vget a with
          | CV0 { ctag0; _ } -> ctag0
          | CV1 ({ ctag1; _ }, _) -> ctag1
        in
        let tag_length, arg_length = case_lengths.(tag) in
        let arg_length =
          match arg_length.of_value with
          | Dynamic f -> f a
          | Static n -> n
          | Unknown ->
              (* [Unknown] arg lengths discounted above *)
              assert false
        in
        tag_length + arg_length
      in
      let of_encoding buf (Size.Offset off) =
        let off = ref off in
        let tag = Bin.Varint.decode buf off in
        match case_lengths.(tag) with
        | _, { of_encoding = Static n; _ } -> Size.Offset (!off + n)
        | _, { of_encoding = Dynamic f; _ } -> f buf (Size.Offset !off)
        | _, { of_encoding = _; _ } -> assert false
      in
      Sizer.dynamic ~of_value ~of_encoding
