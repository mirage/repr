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

include Type_intf
include Type_core
include Staging
open Utils

(* Combinators for Repr types *)

let unit = Prim Unit
let bool = Prim Bool
let char = Prim Char
let int = Prim Int
let int32 = Prim Int32
let int64 = Prim Int64
let float = Prim Float
let string = Prim (String `Int)
let bytes = Prim (Bytes `Int)
let string_of n = Prim (String n)
let bytes_of n = Prim (Bytes n)
let list ?(len = `Int) v = List { v; len }
let array ?(len = `Int) v = Array { v; len }
let pair a b = Tuple (Pair (a, b))
let triple a b c = Tuple (Triple (a, b, c))
let option a = Option a
let boxed t = Boxed t

let abstract ~pp ~of_string ~json ~bin ?unboxed_bin ~equal ~compare ~short_hash
    ~pre_hash () =
  let encode_json, decode_json = json in
  let encode_bin, decode_bin, size_of = bin in
  let unboxed_encode_bin, unboxed_decode_bin, unboxed_size_of =
    match unboxed_bin with None -> bin | Some b -> b
  in
  Custom
    {
      cwit = `Witness (Witness.make ());
      pp;
      of_string;
      pre_hash;
      encode_bin;
      decode_bin;
      size_of;
      compare;
      equal;
      short_hash;
      unboxed_encode_bin;
      unboxed_decode_bin;
      unboxed_size_of;
    }
  |> annotate ~add:Encode_json.add ~data:encode_json
  |> annotate ~add:Decode_json.add ~data:decode_json

(* fix points *)

let mu : type a. (a t -> a t) -> a t =
 fun f ->
  let rec fake_x : a self = { self_unroll = f; self_fix = Self fake_x } in
  let real_x = f (Self fake_x) in
  fake_x.self_fix <- real_x;
  Self fake_x

let mu2 : type a b. (a t -> b t -> a t * b t) -> a t * b t =
 fun f ->
  let rec fake_x =
    let self_unroll a =
      let b = mu (fun b -> f a b |> snd) in
      f a b |> fst
    in
    { self_unroll; self_fix = Self fake_x }
  in
  let rec fake_y =
    let self_unroll b =
      let a = mu (fun a -> f a b |> fst) in
      f a b |> snd
    in
    { self_unroll; self_fix = Self fake_y }
  in
  let real_x, real_y = f (Self fake_x) (Self fake_y) in
  fake_x.self_fix <- real_x;
  fake_y.self_fix <- real_y;
  (Self fake_x, Self fake_y)

(* records *)

type ('a, 'b, 'c) open_record = ('a, 'c) fields -> string * 'b * ('a, 'b) fields

let field fname ftype fget =
  check_valid_utf8 fname;
  { fname; ftype; fget }

let record : string -> 'b -> ('a, 'b, 'b) open_record = fun n c fs -> (n, c, fs)

let app :
    type a b c d.
    (a, b, c -> d) open_record -> (a, c) field -> (a, b, d) open_record =
 fun r f fs ->
  let n, c, fs = r (F1 (f, fs)) in
  (n, c, fs)

module String_Set = Set.Make (String)

(** [check_unique f l] checks that all the strings in [l] are unique. Otherwise,
    calls [f dup] with [dup] the first duplicate. *)
let check_unique f =
  let rec aux set = function
    | [] -> ()
    | x :: xs -> (
        match String_Set.find_opt x set with
        | None -> aux (String_Set.add x set) xs
        | Some _ -> f x)
  in
  aux String_Set.empty

let check_unique_field_names rname rfields =
  let names = List.map (fun (Field { fname; _ }) -> fname) rfields in
  let failure fname =
    Fmt.invalid_arg "The name %s was used for two or more fields in record %s."
      fname rname
  in
  check_unique failure names

let sealr : type a b. (a, b, a) open_record -> a t =
 fun r ->
  let rname, c, fs = r F0 in
  let rwit = Witness.make () in
  let sealed = { rwit; rname; rfields = Fields (fs, c) } in
  check_unique_field_names rname (fields sealed);
  Record sealed

let ( |+ ) = app

(* variants *)

type 'a case_p = 'a case_v
type ('a, 'b) case = int -> 'a a_case * 'b

let case0 cname0 c0 =
  check_valid_utf8 cname0;
  fun ctag0 ->
    let c = { ctag0; cname0; c0 } in
    (C0 c, CV0 c)

let case1 : type a b. string -> b t -> (b -> a) -> (a, b -> a case_p) case =
 fun cname1 ctype1 c1 ->
  check_valid_utf8 cname1;
  fun ctag1 ->
    let cwit1 : b Witness.t = Witness.make () in
    let c = { ctag1; cname1; ctype1; cwit1; c1 } in
    (C1 c, fun v -> CV1 (c, v))

type ('a, 'b, 'c) open_variant = 'a a_case list -> string * 'c * 'a a_case list

let variant n c vs = (n, c, vs)

let app v c cs =
  let n, fc, cs = v cs in
  let c, f = c (List.length cs) in
  (n, fc f, c :: cs)

let check_unique_case_names vname vcases =
  let n0, n1 =
    List.partition (function C0 _ -> true | C1 _ -> false) vcases
  in
  let names0 =
    List.map (function C0 { cname0; _ } -> cname0 | _ -> assert false) n0
  in
  let names1 =
    List.map (function C1 { cname1; _ } -> cname1 | _ -> assert false) n1
  in
  check_unique
    (fun cname ->
      Fmt.invalid_arg
        "The name %s was used for two or more case0 in variant or enum %s."
        cname vname)
    names0;
  check_unique
    (fun cname ->
      Fmt.invalid_arg
        "The name %s was used for two or more case1 in variant or enum %s."
        cname vname)
    names1

let sealv v =
  let vname, vget, vcases = v [] in
  check_unique_case_names vname vcases;
  let vwit = Witness.make () in
  let vcases = Array.of_list (List.rev vcases) in
  Variant { vwit; vname; vcases; vget }

let ( |~ ) = app

type empty = |

(* Encode [empty] as a variant with no constructors *)
let empty = variant "empty" (fun _ -> assert false) |> sealv

let enum vname l =
  let vwit = Witness.make () in
  let _, vcases, mk =
    List.fold_left
      (fun (ctag0, cases, mk) (n, v) ->
        check_valid_utf8 n;
        let c = { ctag0; cname0 = n; c0 = v } in
        (ctag0 + 1, C0 c :: cases, (v, CV0 c) :: mk))
      (0, [], []) l
  in
  check_unique_case_names vname vcases;
  let vcases = Array.of_list (List.rev vcases) in
  Variant { vwit; vname; vcases; vget = (fun x -> List.assq x mk) }

let result a b =
  variant "result" (fun ok error -> function
    | Ok x -> ok x | Error x -> error x)
  |~ case1 "ok" a (fun a -> Ok a)
  |~ case1 "error" b (fun b -> Error b)
  |> sealv

let either a b =
  variant "either" (fun left right -> function
    | Either.Left x -> left x | Either.Right x -> right x)
  |~ case1 "left" a (fun a -> Either.Left a)
  |~ case1 "right" b (fun b -> Either.Right b)
  |> sealv

let pre_hash, encode_bin, decode_bin, to_bin_string, of_bin_string =
  Type_binary.(pre_hash, encode_bin, decode_bin, to_bin_string, of_bin_string)

let short_hash = function
  | Custom c -> stage c.short_hash
  | t ->
      let pre_hash = unstage (pre_hash t) in
      stage @@ fun ?seed x ->
      let seed = match seed with None -> 0 | Some t -> t in
      let h = ref seed in
      pre_hash x (fun s -> h := Hashtbl.seeded_hash !h s);
      !h

exception Unsupported_operation of string

let undefined name _ = raise (Unsupported_operation name)

type 'a impl = Structural | Custom of 'a | Undefined

let fold_impl ~undefined ~structural = function
  | Custom x -> x
  | Undefined -> undefined ()
  | Structural -> structural ()

let partially_abstract ~pp ~of_string ~json ~bin ~unboxed_bin ~equal ~compare
    ~short_hash:short_hash_t ~pre_hash:pre_hash_t t : _ t =
  let encode_json, decode_json =
    fold_impl json
      ~undefined:(fun () -> (undefined "encode_json", undefined "decode_json"))
      ~structural:(fun () ->
        let rec is_prim : type a. a t -> bool = function
          | Self s -> is_prim s.self_fix
          | Map m -> is_prim m.x
          | Prim _ -> true
          | _ -> false
        in
        match (t, pp, of_string) with
        | ty, Custom pp, Custom of_string when is_prim ty ->
            let ( >|= ) x f = Result.map f x in
            let join = function Error _ as e -> e | Ok x -> x in
            let ty = string in
            let encode ppf u =
              Type_json.encode ty ppf (Fmt.to_to_string pp u)
            in
            let decode buf = Type_json.decode ty buf >|= of_string |> join in
            (encode, decode)
        | _ -> (Type_json.encode t, Type_json.decode t))
  in
  let pp =
    fold_impl pp
      ~structural:(fun () -> Type_pp.t t)
      ~undefined:(fun () -> undefined "pp")
  in
  let of_string =
    fold_impl of_string
      ~structural:(fun () -> Type_pp.of_string t)
      ~undefined:(fun () -> undefined "of_string")
  in
  let encode_bin, decode_bin, size_of =
    fold_impl bin
      ~undefined:(fun () ->
        (undefined "encode_bin", undefined "decode_bin", unimplemented_size_of))
      ~structural:(fun () ->
        ( unstage (Type_binary.encode_bin t),
          unstage (Type_binary.decode_bin t),
          Type_size.t t ))
  in
  let unboxed_encode_bin, unboxed_decode_bin, unboxed_size_of =
    fold_impl unboxed_bin
      ~undefined:(fun () ->
        ( undefined "Unboxed.encode_bin",
          undefined "Unboxed.decode_bin",
          unimplemented_size_of ))
      ~structural:(fun () ->
        ( unstage (Type_binary.Unboxed.encode_bin t),
          unstage (Type_binary.Unboxed.decode_bin t),
          Type_size.unboxed t ))
  in
  let equal =
    fold_impl equal
      ~undefined:(fun () -> undefined "equal")
      ~structural:(fun () -> unstage (Type_ordered.equal t))
  in
  let compare =
    fold_impl compare
      ~undefined:(fun () -> undefined "compare")
      ~structural:(fun () -> unstage (Type_ordered.compare t))
  in
  let short_hash =
    fold_impl short_hash_t
      ~undefined:(fun () ?seed:_ -> undefined "short_hash" ())
      ~structural:(fun () -> unstage (short_hash t))
  in
  let pre_hash =
    fold_impl pre_hash_t
      ~undefined:(fun () -> undefined "pre_hash")
      ~structural:(fun () -> encode_bin)
  in
  Type_core.Custom
    {
      cwit = `Type t;
      pp;
      of_string;
      encode_bin;
      decode_bin;
      size_of;
      compare;
      equal;
      short_hash;
      pre_hash;
      unboxed_encode_bin;
      unboxed_decode_bin;
      unboxed_size_of;
    }
  |> annotate ~add:Encode_json.add ~data:encode_json
  |> annotate ~add:Decode_json.add ~data:decode_json

let like ?pp ?of_string ?json ?bin ?unboxed_bin ?equal ?compare ?short_hash
    ?pre_hash t =
  let to_impl = function None -> Structural | Some x -> Custom x in
  let equal =
    match equal with
    | Some x -> Custom x
    | None -> (
        match compare with
        | Some f -> Custom (fun x y -> f x y = 0)
        | None -> Structural)
  in
  let pp = to_impl pp
  and json = to_impl json
  and of_string = to_impl of_string
  and bin = to_impl bin
  and unboxed_bin = to_impl unboxed_bin
  and compare = to_impl compare
  and short_hash = to_impl short_hash
  and pre_hash = to_impl pre_hash in
  partially_abstract ~pp ~json ~of_string ~bin ~unboxed_bin ~equal ~compare
    ~short_hash ~pre_hash t

let map ?pp ?of_string ?json ?bin ?unboxed_bin ?equal ?compare ?short_hash
    ?pre_hash x f g =
  match
    (pp, of_string, json, bin, unboxed_bin, equal, compare, short_hash, pre_hash)
  with
  | None, None, None, None, None, None, None, None, None ->
      Map { x; f; g; mwit = Witness.make () }
  | _ ->
      let x = Map { x; f; g; mwit = Witness.make () } in
      like ?pp ?of_string ?json ?bin ?unboxed_bin ?equal ?compare ?short_hash
        ?pre_hash x

module type S = sig
  type t

  val t : t ty
end

let equal, compare = Type_ordered.(equal, compare)

let pp, pp_dump, pp_ty, to_string, of_string =
  Type_pp.(t, dump, ty, to_string, of_string)

let ( to_json_string,
      of_json_string,
      pp_json,
      encode_json,
      decode_json,
      decode_json_lexemes ) =
  Type_json.(to_string, of_string, pp, encode, decode_jsonm, decode_lexemes)

let random, random_state = Type_random.(of_global, of_state)

let size_of t =
  match (Type_size.t t).of_value with
  | Size.Static n ->
      let n = Some n in
      stage (fun _ -> n)
  | Size.Dynamic f -> stage (fun x -> Some (f x))
  | Size.Unknown -> stage (fun _ -> None)

module Size = struct
  type 'a t = 'a Size.t = Static of int | Dynamic of 'a | Unknown

  (* The [Size] module defines _scanning_ length decoders that return
     [initial_offset + length] rather than just [length]. These functions
     convert these to decoders that return the [length] directly. *)

  let to_scanning : type a. (a -> int -> int) -> a -> Size.offset -> Size.offset
      =
   fun len_f buf (Size.Offset off) -> Size.Offset (off + len_f buf off)

  let of_scanning : type a. (a -> Size.offset -> Size.offset) -> a -> int -> int
      =
   fun scan_f buf off ->
    let (Size.Offset off') = scan_f buf (Size.Offset off) in
    off' - off

  let of_value t = (Type_size.t t).of_value
  let of_encoding t = Size.map of_scanning (Type_size.unboxed t).of_encoding
  let t t = Type_size.t t

  type 'a sizer = 'a size_of

  let using f t =
    let of_value =
      Size.map (fun sizer x -> sizer (f x)) t.Size.Sizer.of_value
    in
    { t with of_value }

  let custom_static n =
    Size.Sizer.{ of_value = Static n; of_encoding = Static n }

  let custom_dynamic ?of_value ?of_encoding () =
    let of_value =
      match of_value with Some f -> Dynamic f | None -> Unknown
    in
    let of_encoding =
      match of_encoding with
      | Some f -> Dynamic (to_scanning f)
      | None -> Unknown
    in
    Size.Sizer.{ of_value; of_encoding }
end

module Unboxed = struct
  include Type_binary.Unboxed

  let size_of = Type_size.unboxed
end

module Json = struct
  include Json

  let assoc : type a. a t -> (string * a) list t =
   fun a ->
    let json = (Type_json.encode_assoc a, Type_json.decode_assoc a) in
    list (pair string a) |> like ~json
end

module Attribute = struct
  let set_random f ty = annotate ~add:Type_random.Attr.add ~data:f ty
end

let int63 =
  let module I = Optint.Int63 in
  let random : Stdlib.Random.State.t -> I.t =
    match I.is_immediate with
    | True -> unstage (random_state int)
    | False ->
        let random_int64 = unstage (random_state int64) in
        fun s -> I.of_int64 (Int64.shift_right (random_int64 s) 1)
  in
  like ~pp:I.pp ~equal:I.equal ~compare:I.compare
    (map int64 I.of_int64 I.to_int64)
  |> Attribute.set_random random

let ref : type a. a t -> a ref t = fun a -> map a ref (fun t -> !t)
let lazy_t : type a. a t -> a Lazy.t t = fun a -> map a Lazy.from_val Lazy.force

let seq : type a. a t -> a Seq.t t =
 fun a ->
  let elt_equal = unstage @@ equal a in
  let elt_compare = unstage @@ compare a in
  let rec compare (s1 : a Seq.t) (s2 : a Seq.t) =
    match (s1 (), s2 ()) with
    | Nil, Nil -> 0
    | Cons _, Nil -> 1
    | Nil, Cons _ -> -1
    | Cons (x, xf), Cons (y, yf) ->
        let ord = elt_compare x y in
        if ord <> 0 then ord else compare xf yf
  in
  let rec equal (s1 : a Seq.t) (s2 : a Seq.t) =
    match (s1 (), s2 ()) with
    | Nil, Nil -> true
    | Cons _, Nil | Nil, Cons _ -> false
    | Cons (x, xf), Cons (y, yf) -> elt_equal x y && equal xf yf
  in
  map ~compare ~equal (list a) List.to_seq List.of_seq

let stack : type a. a t -> a Stack.t t =
  (* Built-in [Stack.of_seq] adds elements bottom-to-top, which flips the stack.
     We must re-flip it afterwards. *)
  let flip_stack s_rev =
    let s = Stack.create () in
    Stack.iter (fun a -> Stack.push a s) s_rev;
    s
  in
  fun a -> map (seq a) (fun s -> Stack.of_seq s |> flip_stack) Stack.to_seq

let queue : type a. a t -> a Queue.t t =
 fun a -> map (seq a) Queue.of_seq Queue.to_seq

let hashtbl : type k v. k t -> v t -> (k, v) Hashtbl.t t =
 fun k v -> map (seq (pair k v)) Hashtbl.of_seq Hashtbl.to_seq

let set (type set elt) (module Set : Set.S with type elt = elt and type t = set)
    (elt : elt t) : set t =
  map (seq elt) Set.of_seq Set.to_seq

module Of_set (Set : sig
  include Set.S

  val elt_t : elt ty
end) =
struct
  let t = set (module Set) Set.elt_t
end

module Of_map (Map : sig
  include Map.S

  val key_t : key ty
end) =
struct
  let t : type v. v t -> v Map.t t =
   fun v -> map (seq (pair Map.key_t v)) Map.of_seq Map.to_seq
end
