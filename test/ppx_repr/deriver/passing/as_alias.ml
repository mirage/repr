type 'a typ = 'a Repr.t

module Trivial : sig
  type t [@@deriving repr]
end = struct
  type t = int as 'a [@@deriving repr]
end

module Recursive : sig
  type 'a tree [@@deriving repr]
end = struct
  type 'a tree = [ `Branch of 'tree * int * 'tree | `Leaf of 'a ] as 'tree
  [@@deriving repr]
end
