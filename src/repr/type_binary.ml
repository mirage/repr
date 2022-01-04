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
open Staging
open Utils
module Bin = Binary

module Encode = struct
  type 'a encoder = 'a encode_bin staged

  let string boxed n =
    if boxed then Bin.String.encode n else Bin.String_unboxed.encode n

  let bytes boxed n =
    if boxed then Bin.Bytes.encode n else Bin.Bytes_unboxed.encode n

  let list l n =
    let l = unstage l in
    Bin.List.encode n l

  let array l n =
    let l = unstage l in
    Bin.Array.encode n l

  let pair a b =
    let a = unstage a and b = unstage b in
    stage (Bin.Pair.encode a b)

  let triple a b c =
    let a = unstage a and b = unstage b and c = unstage c in
    stage (Bin.Triple.encode a b c)

  let option o =
    let o = unstage o in
    stage (Bin.Option.encode o)

  let rec t : type a. a t -> a encoder = function
    | Self s -> fst (self s)
    | Custom c -> stage c.encode_bin
    | Map b -> map ~boxed:true b
    | Prim t -> prim ~boxed:true t
    | Boxed b -> t b
    | Attributes { attr_type = x; _ } -> t x
    | List l -> list (t l.v) l.len
    | Array a -> array (t a.v) a.len
    | Tuple t -> tuple t
    | Option x -> option (t x)
    | Record r -> record r
    | Variant v -> variant v
    | Var v -> raise (Unbound_type_variable v)

  and unboxed : type a. a t -> a encoder = function
    | Self s -> snd (self s)
    | Custom c -> stage c.unboxed_encode_bin
    | Map b -> map ~boxed:false b
    | Prim t -> prim ~boxed:false t
    | Boxed b -> t b
    | Attributes { attr_type = x; _ } -> unboxed x
    | List l -> list (t l.v) l.len
    | Array a -> array (t a.v) a.len
    | Tuple t -> tuple t
    | Option x -> option (t x)
    | Record r -> record r
    | Variant v -> variant v
    | Var v -> raise (Unbound_type_variable v)

  and self : type a. a self -> a encoder * a encoder =
   fun { self_unroll; _ } ->
    fix_staged2 (fun encode_bin unboxed_encode_bin ->
        let cyclic =
          self_unroll
            (partial ~encode_bin:(unstage encode_bin)
               ~unboxed_encode_bin:(unstage unboxed_encode_bin)
               ())
        in
        (t cyclic, unboxed cyclic))

  and tuple : type a. a tuple -> a encoder = function
    | Pair (x, y) -> pair (t x) (t y)
    | Triple (x, y, z) -> triple (t x) (t y) (t z)

  and map : type a b. boxed:bool -> (a, b) map -> b encoder =
   fun ~boxed { x; g; _ } ->
    let encode_bin = unstage (if boxed then t x else unboxed x) in
    stage (fun u k -> encode_bin (g u) k)

  and prim : type a. boxed:bool -> a prim -> a encoder =
   fun ~boxed -> function
    | Unit -> stage Bin.Unit.encode
    | Bool -> stage Bin.Bool.encode
    | Char -> stage Bin.Char.encode
    | Int -> stage Bin.Varint.encode
    | Int32 -> stage Bin.Int32.encode
    | Int64 -> stage Bin.Int64.encode
    | Float -> stage Bin.Float.encode
    | String n -> string boxed n
    | Bytes n -> bytes boxed n

  and record : type a. a record -> a encoder =
   fun r ->
    let field_encoders : (a -> (string -> unit) -> unit) list =
      ListLabels.map (fields r) ~f:(fun (Field f) ->
          let field_encode = unstage (t f.ftype) in
          fun x -> field_encode (f.fget x))
    in
    stage (fun x k -> Stdlib.List.iter (fun f -> f x k) field_encoders)

  and variant : type a. a variant -> a encoder =
    let c0 { ctag0; _ } = stage (Bin.Varint.encode ctag0) in
    let c1 c =
      let encode_arg = unstage (t c.ctype1) in
      stage (fun v k ->
          Bin.Varint.encode c.ctag1 k;
          encode_arg v k)
    in
    fun v -> fold_variant { c0; c1 } v
end

module Decode = struct
  type 'a decoder = 'a decode_bin staged

  let string box = if box then Bin.String.decode else Bin.String_unboxed.decode
  let bytes box = if box then Bin.Bytes.decode else Bin.Bytes_unboxed.decode

  let list l n =
    let l = unstage l in
    Bin.List.decode n l

  let array l n =
    let l = unstage l in
    Bin.Array.decode n l

  let pair a b =
    let a = unstage a and b = unstage b in
    stage (Bin.Pair.decode a b)

  let triple a b c =
    let a = unstage a and b = unstage b and c = unstage c in
    stage (Bin.Triple.decode a b c)

  let option o =
    let o = unstage o in
    stage (Bin.Option.decode o)

  module Record_decoder = Fields_folder (struct
    type ('a, 'b) t = string -> int ref -> 'b -> 'a
  end)

  let rec t : type a. a t -> a decoder = function
    | Self s -> fst (self s)
    | Custom c -> stage c.decode_bin
    | Map b -> map ~boxed:true b
    | Prim t -> prim ~boxed:true t
    | Boxed b -> t b
    | Attributes { attr_type = x; _ } -> t x
    | List l -> list (t l.v) l.len
    | Array a -> array (t a.v) a.len
    | Tuple t -> tuple t
    | Option x -> option (t x)
    | Record r -> record r
    | Variant v -> variant v
    | Var v -> raise (Unbound_type_variable v)

  and unboxed : type a. a t -> a decoder = function
    | Self s -> snd (self s)
    | Custom c -> stage c.unboxed_decode_bin
    | Map b -> map ~boxed:false b
    | Prim t -> prim ~boxed:false t
    | Boxed b -> t b
    | Attributes { attr_type = x; _ } -> unboxed x
    | List l -> list (t l.v) l.len
    | Array a -> array (t a.v) a.len
    | Tuple t -> tuple t
    | Option x -> option (t x)
    | Record r -> record r
    | Variant v -> variant v
    | Var v -> raise (Unbound_type_variable v)

  and self : type a. a self -> a decoder * a decoder =
   fun { self_unroll; _ } ->
    fix_staged2 (fun decode_bin unboxed_decode_bin ->
        let cyclic =
          self_unroll
            (partial ~decode_bin:(unstage decode_bin)
               ~unboxed_decode_bin:(unstage unboxed_decode_bin)
               ())
        in
        (t cyclic, unboxed cyclic))

  and tuple : type a. a tuple -> a decoder = function
    | Pair (x, y) -> pair (t x) (t y)
    | Triple (x, y, z) -> triple (t x) (t y) (t z)

  and map : type a b. boxed:bool -> (a, b) map -> b decoder =
   fun ~boxed { x; f; _ } ->
    let decode_bin = unstage (if boxed then t x else unboxed x) in
    stage (fun buf pos_ref -> f (decode_bin buf pos_ref))

  and prim : type a. boxed:bool -> a prim -> a decoder =
   fun ~boxed -> function
    | Unit -> stage Bin.Unit.decode
    | Bool -> stage Bin.Bool.decode
    | Char -> stage Bin.Char.decode
    | Int -> stage Bin.Varint.decode
    | Int32 -> stage Bin.Int32.decode
    | Int64 -> stage Bin.Int64.decode
    | Float -> stage Bin.Float.decode
    | String n -> string boxed n
    | Bytes n -> bytes boxed n

  and record : type a. a record -> a decoder =
   fun { rfields = Fields (fs, constr); _ } ->
    let nil _buf _pos_ref f = f in
    let cons { ftype; _ } decode_remaining =
      let f_decode = unstage (t ftype) in
      fun buf pos_ref constr ->
        let x = f_decode buf pos_ref in
        let constr = constr x in
        decode_remaining buf pos_ref constr
    in
    let f = Record_decoder.fold { nil; cons } fs in
    stage (fun buf pos_ref -> f buf pos_ref constr)

  and variant : type a. a variant -> a decoder =
   fun v ->
    let decoders : a decoder array =
      ArrayLabels.map v.vcases ~f:(function
        | C0 c -> stage (fun _ _ -> c.c0)
        | C1 c ->
            let decode_arg = unstage (t c.ctype1) in
            stage (fun buf pos_ref -> c.c1 (decode_arg buf pos_ref)))
    in
    stage (fun buf pos_ref ->
        let i = Bin.Varint.decode buf pos_ref in
        unstage decoders.(i) buf pos_ref)
end

module Pre_hash = struct
  type 'a pre_hash = 'a encode_bin staged

  let rec t : type a. a t -> a pre_hash = function
    | Self s -> self s
    | Custom c -> stage c.pre_hash
    | Map m -> map m
    | Boxed b -> t b
    | Attributes { attr_type; _ } -> t attr_type
    | List l -> Encode.list (t l.v) l.len
    | Array a -> Encode.array (t a.v) a.len
    | Tuple t -> tuple t
    | Option x -> Encode.option (t x)
    | Record r -> record r
    | Variant v -> variant v
    | Var v -> raise (Unbound_type_variable v)
    | Prim _ as t -> Encode.t t

  and self : type a. a self -> a pre_hash =
   fun { self_unroll; _ } ->
    fix_staged (fun pre_hash ->
        let cyclic = self_unroll (partial ~pre_hash:(unstage pre_hash) ()) in
        t cyclic)

  and tuple : type a. a tuple -> a pre_hash = function
    | Pair (x, y) -> Encode.pair (t x) (t y)
    | Triple (x, y, z) -> Encode.triple (t x) (t y) (t z)

  and map : type a b. (a, b) map -> b pre_hash =
   fun { x; g; _ } ->
    let pre_hash = unstage (t x) in
    stage (fun u f -> pre_hash (g u) f)

  and record : type a. a record -> a pre_hash =
   fun r ->
    let field_encoders : (a -> (string -> unit) -> unit) list =
      ListLabels.map (fields r) ~f:(fun (Field f) ->
          let field_encode = unstage (t f.ftype) in
          fun x -> field_encode (f.fget x))
    in
    stage (fun x k -> Stdlib.List.iter (fun f -> f x k) field_encoders)

  and variant : type a. a variant -> a pre_hash =
    let c0 { ctag0; _ } = stage (Bin.Varint.encode ctag0) in
    let c1 c =
      let encode_arg = unstage (t c.ctype1) in
      stage (fun v k ->
          Bin.Varint.encode c.ctag1 k;
          encode_arg v k)
    in
    fun v -> fold_variant { c0; c1 } v

  (* NOTE: for compatibility reasons, the pre-hash of a primitive type uses the
     _unboxed_ encoding of that type (but primitive components of larger types
     _are_ boxed). *)
  let t =
    let rec aux : type a. a t -> a pre_hash = function
      | Prim _ as ty -> Encode.unboxed ty
      (* 'Simple' wrappers around primitive types retain the unboxed property: *)
      | Attributes { attr_type; _ } -> aux attr_type
      | Self s -> aux s.self_fix
      | Map m ->
          let dst = unstage (aux m.x) in
          stage (fun v -> dst (m.g v))
      (* Otherwise, use regular boxed pre-hashing: *)
      | ty -> t ty
    in
    aux
end

let encode_bin = Encode.t
let decode_bin = Decode.t
let pre_hash = Pre_hash.t

type 'a to_bin_string = 'a to_string staged
type 'a of_bin_string = 'a of_string staged

module Unboxed = struct
  let encode_bin = Encode.unboxed
  let decode_bin = Decode.unboxed
end

let to_bin (size_of : _ Size.Sizer.t) encode_bin =
  let encode_bin = unstage encode_bin in
  stage (fun x ->
      let seq = encode_bin x in
      let len =
        match size_of.of_value with
        | Static n -> n
        | Dynamic f -> f x
        | Unknown -> 1024
      in
      let buf = Buffer.create len in
      seq (Buffer.add_string buf);
      Buffer.contents buf)

let to_bin_string =
  let rec aux : type a. a t -> a to_bin_string =
   fun t ->
    match t with
    | Self s -> aux s.self_fix
    | Map m ->
        let mapped = unstage (aux m.x) in
        stage (fun x -> mapped (m.g x))
    | Prim (String _) -> stage (fun x -> x)
    | Prim (Bytes _) -> stage Bytes.to_string
    | Custom c -> to_bin c.unboxed_size_of (stage c.unboxed_encode_bin)
    | _ -> to_bin (Type_size.unboxed t) (Encode.unboxed t)
  in
  aux

let map_result f = function Ok x -> Ok (f x) | Error _ as e -> e

let of_bin decode_bin x =
  let pos_ref = ref 0 in
  let v = decode_bin x pos_ref in
  assert (!pos_ref = String.length x);
  Ok v

let of_bin_string t =
  let rec aux : type a. a t -> a of_bin_string =
   fun t ->
    match t with
    | Self s -> aux s.self_fix
    | Map l ->
        let mapped = unstage (aux l.x) in
        stage (fun x -> mapped x |> map_result l.f)
    | Prim (String _) -> stage (fun x -> Ok x)
    | Prim (Bytes _) -> stage (fun x -> Ok (Bytes.of_string x))
    | Custom c -> stage (of_bin c.unboxed_decode_bin)
    | _ -> stage (of_bin (unstage (Decode.unboxed t)))
  in
  let f = unstage (aux t) in
  stage (fun x -> try f x with Invalid_argument e -> Error (`Msg e))
