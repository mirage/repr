(* Tests of the signature deriver *)
module SigTests : sig
  type t = string [@@deriving repr]
  type foo = unit [@@deriving repr { name = "foo_repr" }]
  type my_int = int32 * t [@@deriving repr]

  type my_variant =
    | A of (my_int, int) result
    | B of unit
    | C of string * int32
  [@@deriving repr]
end = struct
  type t = string [@@deriving repr]
  type foo = unit [@@deriving repr { name = "foo_repr" }]
  type my_int = int32 * t [@@deriving repr]

  type my_variant =
    | A of (my_int, int) result
    | B of unit
    | C of string * int32
  [@@deriving repr]
end
