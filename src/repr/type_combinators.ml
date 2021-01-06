include Type_core
include Type_combinators_intf
open Utils

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
