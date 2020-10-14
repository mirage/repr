(* Tests of the arguments/attributes *)
type c = string [@@deriving repr { name = "c_wit" }]

let (_ : c Repr.t) = c_wit

type d = int [@@deriving repr { name = "repr_for_d" }]

let (_ : d Repr.t) = repr_for_d

type point_elsewhere1 = (c[@repr c_wit]) [@@deriving repr]
type point_elsewhere2 = int * (c[@repr c_wit]) [@@deriving repr]

type point_elsewhere3 = A of int * (c[@repr c_wit]) | B of (c[@repr c_wit])
[@@deriving repr]

type point_elsewhere4 = {
  lorem : string;
  ipsum : (c[@repr c_wit]);
  dolor : int;
  sit : (d[@repr repr_for_d]);
}
[@@deriving repr]

let (_ : point_elsewhere1 Repr.t) = point_elsewhere1_t
let (_ : point_elsewhere2 Repr.t) = point_elsewhere2_t
let (_ : point_elsewhere3 Repr.t) = point_elsewhere3_t
let (_ : point_elsewhere4 Repr.t) = point_elsewhere4_t
