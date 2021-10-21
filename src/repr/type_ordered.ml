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

module Refl = struct
  open Witness

  let prim : type a b. a prim -> b prim -> (a, b) eq option =
   fun a b ->
    match (a, b) with
    | Unit, Unit -> Some Refl
    | Bool, Bool -> Some Refl
    | Char, Char -> Some Refl
    | Int, Int -> Some Refl
    | Int32, Int32 -> Some Refl
    | Int64, Int64 -> Some Refl
    | Float, Float -> Some Refl
    | String _, String _ -> Some Refl
    | Bytes _, Bytes _ -> Some Refl
    | _ -> None

  let rec t : type a b. a ty -> b ty -> (a, b) eq option =
   fun a b ->
    match (a, b) with
    | Self a, _ -> t a.self_fix b
    | _, Self b -> t a b.self_fix
    | Map a, Map b -> Witness.eq a.mwit b.mwit
    | Custom a, Custom b -> Witness.eq a.cwit b.cwit
    | Prim a, Prim b -> prim a b
    | Array a, Array b -> (
        match t a.v b.v with Some Refl -> Some Refl | None -> None)
    | List a, List b -> (
        match t a.v b.v with Some Refl -> Some Refl | None -> None)
    | Tuple a, Tuple b -> tuple a b
    | Option a, Option b -> (
        match t a b with Some Refl -> Some Refl | None -> None)
    | Record a, Record b -> Witness.eq a.rwit b.rwit
    | Variant a, Variant b -> Witness.eq a.vwit b.vwit
    | Var v, _ | _, Var v -> raise (Unbound_type_variable v)
    | Attributes a, Attributes b -> t a.attr_type b.attr_type
    | Boxed a, b -> t a b
    | a, Boxed b -> t a b
    | ( ( Map _ | Custom _ | Prim _ | Array _ | List _ | Tuple _ | Option _
        | Record _ | Variant _ | Attributes _ ),
        _ ) ->
        None

  and tuple : type a b. a tuple -> b tuple -> (a, b) eq option =
   fun a b ->
    match (a, b) with
    | Pair (a0, a1), Pair (b0, b1) -> (
        match (t a0 b0, t a1 b1) with
        | Some Refl, Some Refl -> Some Refl
        | _ -> None)
    | Triple (a0, a1, a2), Triple (b0, b1, b2) -> (
        match (t a0 b0, t a1 b1, t a2 b2) with
        | Some Refl, Some Refl, Some Refl -> Some Refl
        | _ -> None)
    | _ -> None
end

module Equal = struct
  module Attr = Attribute.Make1 (struct
    type 'a t = 'a equal

    let name = "equal"
  end)

  let unit _ _ = true
  let bool (x : bool) (y : bool) = x = y
  let char (x : char) (y : char) = x = y
  let int (x : int) (y : int) = x = y
  let int32 (x : int32) (y : int32) = x = y
  let int64 (x : int64) (y : int64) = x = y
  let string x y = x == y || String.equal x y
  let bytes x y = x == y || Bytes.equal x y

  (* NOTE: equality is ill-defined on float *)
  let float (x : float) (y : float) = x = y

  let list e =
    let e = unstage e in
    stage @@ fun x y ->
    x == y || (List.length x = List.length y && List.for_all2 e x y)

  let array e =
    let e = unstage e in
    stage @@ fun x y ->
    x == y
    || Array.length x = Array.length y
       &&
       let rec aux = function
         | -1 -> true
         | i -> e x.(i) y.(i) && aux (i - 1)
       in
       aux (Array.length x - 1)

  let pair ex ey =
    let ex = unstage ex and ey = unstage ey in
    stage @@ fun ((x1, y1) as a) ((x2, y2) as b) ->
    a == b || (ex x1 x2 && ey y1 y2)

  let triple ex ey ez =
    let ex = unstage ex and ey = unstage ey and ez = unstage ez in
    stage @@ fun ((x1, y1, z1) as a) ((x2, y2, z2) as b) ->
    a == b || (ex x1 x2 && ey y1 y2 && ez z1 z2)

  let option e =
    let e = unstage e in
    stage @@ fun x y ->
    x == y
    ||
    match (x, y) with
    | None, None -> true
    | Some x, Some y -> e x y
    | _ -> false

  let rec t : type a. a t -> a equal staged = function
    | Self s -> self s
    | Custom c -> stage c.equal
    | Map m -> map m
    | Attributes { attrs; attr_type } -> (
        match Attr.find attrs with Some f -> stage f | None -> t attr_type)
    | Boxed x -> t x
    | Prim p -> prim p
    | List l -> list (t l.v)
    | Array x -> array (t x.v)
    | Tuple t -> tuple t
    | Option x -> option (t x)
    | Record r -> record r
    | Variant v -> variant v
    | Var v -> raise (Unbound_type_variable v)

  and self : type a. a self -> a equal staged = function
    | { self_unroll; _ } ->
        fix_staged (fun equal ->
            let cyclic = self_unroll (partial ~equal:(unstage equal) ()) in
            t cyclic)

  and tuple : type a. a tuple -> a equal staged = function
    | Pair (a, b) -> pair (t a) (t b)
    | Triple (a, b, c) -> triple (t a) (t b) (t c)

  and map : type a b. (a, b) map -> b equal staged =
   fun { x; g; _ } ->
    let eq = unstage (t x) in
    stage @@ fun u v -> eq (g u) (g v)

  and prim : type a. a prim -> a equal staged = function
    | Unit -> stage unit
    | Bool -> stage bool
    | Char -> stage char
    | Int -> stage int
    | Int32 -> stage int32
    | Int64 -> stage int64
    | Float -> stage float
    | String _ -> stage string
    | Bytes _ -> stage bytes

  and record : type a. a record -> a equal staged =
   fun r ->
    let fields =
      List.map (function Field f -> unstage (field f)) (fields r)
    in
    stage @@ fun x y -> List.for_all (function f -> f x y) fields

  and field : type a b. (a, b) field -> a equal staged =
   fun f ->
    let equal = unstage (t f.ftype) in
    stage @@ fun x y -> equal (f.fget x) (f.fget y)

  and variant : type a. a variant -> a equal staged =
   fun v ->
    let equal x y =
      match (x, y) with
      | CV0 x, CV0 y -> int x.ctag0 y.ctag0
      | CV1 (x, vx), CV1 (y, vy) ->
          int x.ctag1 y.ctag1 && eq (x.ctype1, vx) (y.ctype1, vy)
      | _ -> false
    in
    stage @@ fun x y -> equal (v.vget x) (v.vget y)

  and eq : type a b. a t * a -> b t * b -> bool =
   fun (tx, x) (ty, y) ->
    match Refl.t tx ty with
    | Some Witness.Refl -> unstage (t tx) x y
    | None -> assert false

  (* this should never happen *)
end

module Compare = struct
  module Attr = Attribute.Make1 (struct
    type 'a t = 'a compare

    let name = "compare"
  end)

  let unit (_ : unit) (_ : unit) = 0 [@@inline always]
  let bool (x : bool) (y : bool) = compare x y [@@inline always]
  let char x y = Char.compare x y [@@inline always]
  let int (x : int) (y : int) = compare x y [@@inline always]
  let int32 x y = Int32.compare x y [@@inline always]
  let int64 x y = Int64.compare x y [@@inline always]
  let float (x : float) (y : float) = compare x y [@@inline always]
  let string x y = if x == y then 0 else String.compare x y [@@inline always]
  let bytes x y = if x == y then 0 else Bytes.compare x y [@@inline always]

  let list c =
    let c = unstage c in
    stage @@ fun x y ->
    if x == y then 0
    else
      let rec aux x y =
        match (x, y) with
        | [], [] -> 0
        | [], _ -> -1
        | _, [] -> 1
        | xx :: x, yy :: y -> ( match c xx yy with 0 -> aux x y | i -> i)
      in
      aux x y

  let array c =
    let c = unstage c in
    stage @@ fun x y ->
    if x == y then 0
    else
      let lenx = Array.length x in
      let leny = Array.length y in
      if lenx > leny then 1
      else if lenx < leny then -1
      else
        let rec aux i =
          match c x.(i) y.(i) with
          | 0 when i + 1 = lenx -> 0
          | 0 -> aux (i + 1)
          | i -> i
        in
        aux 0

  let pair cx cy =
    let cx = unstage cx and cy = unstage cy in
    stage @@ fun ((x1, y1) as a) ((x2, y2) as b) ->
    if a == b then 0 else match cx x1 x2 with 0 -> cy y1 y2 | i -> i

  let triple cx cy cz =
    let cx = unstage cx in
    let pair = unstage (pair cy cz) in
    stage @@ fun ((x1, y1, z1) as a) ((x2, y2, z2) as b) ->
    if a == b then 0
    else match cx x1 x2 with 0 -> pair (y1, z1) (y2, z2) | i -> i

  let option c =
    let c = unstage c in
    stage @@ fun x y ->
    if x == y then 0
    else
      match (x, y) with
      | None, None -> 0
      | Some _, None -> 1
      | None, Some _ -> -1
      | Some x, Some y -> c x y

  let prim : type a. a prim -> a compare staged = function
    | Unit -> stage unit
    | Bool -> stage bool
    | Char -> stage char
    | Int -> stage int
    | Int32 -> stage int32
    | Int64 -> stage int64
    | Float -> stage float
    | String _ -> stage string
    | Bytes _ -> stage bytes
    [@@inline always]

  let rec t : type a. a t -> a compare staged = function
    | Self s -> self s
    | Custom c -> stage c.compare
    | Map m -> map m
    | Boxed x -> t x
    | Attributes { attrs; attr_type } -> (
        match Attr.find attrs with Some f -> stage f | None -> t attr_type)
    | Prim p -> (prim [@inlined]) p
    | List l -> list (t l.v)
    | Array x -> array (t x.v)
    | Tuple t -> tuple t
    | Option x -> option (t x)
    | Record r -> record r
    | Variant v -> variant v
    | Var v -> raise (Unbound_type_variable v)

  and self : type a. a self -> a compare staged = function
    | { self_unroll; _ } ->
        fix_staged (fun compare ->
            let cyclic = self_unroll (partial ~compare:(unstage compare) ()) in
            t cyclic)

  and tuple : type a. a tuple -> a compare staged = function
    | Pair (x, y) -> pair (t x) (t y)
    | Triple (x, y, z) -> triple (t x) (t y) (t z)

  and map : type a b. (a, b) map -> b compare staged =
   fun { x; g; _ } ->
    let compare = unstage (t x) in
    stage @@ fun u v -> compare (g u) (g v)

  and record : type a. a record -> a compare staged =
   fun r ->
    let fields =
      List.map (function Field f -> unstage (field f)) (fields r)
    in
    stage @@ fun x y ->
    let rec aux = function
      | [] -> 0
      | f :: t -> ( match f x y with 0 -> aux t | i -> i)
    in
    aux fields

  and field : type a b. (a, b) field -> a compare staged =
   fun f ->
    let compare = unstage (t f.ftype) in
    stage @@ fun x y -> compare (f.fget x) (f.fget y)

  and variant : type a. a variant -> a compare staged =
   fun v ->
    let compare x y =
      match (x, y) with
      | CV0 x, CV0 y -> int x.ctag0 y.ctag0
      | CV0 x, CV1 (y, _) -> int x.ctag0 y.ctag1
      | CV1 (x, _), CV0 y -> int x.ctag1 y.ctag0
      | CV1 (x, vx), CV1 (y, vy) -> (
          match int x.ctag1 y.ctag1 with
          | 0 -> compare (x.ctype1, vx) (y.ctype1, vy)
          | i -> i)
    in
    stage @@ fun x y -> compare (v.vget x) (v.vget y)

  and compare : type a b. a t * a -> b t * b -> int =
   fun (tx, x) (ty, y) ->
    match Refl.t tx ty with
    | Some Witness.Refl -> unstage (t tx) x y
    | None -> assert false

  (* this should never happen *)
end
