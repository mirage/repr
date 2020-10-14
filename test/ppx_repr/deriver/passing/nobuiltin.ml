(* When a type is annotated with the [nobuiltin] annotation, it should be
   considered as an abstract type (i.e. don't pull representations from
   [Repr]). *)

type unit = string [@@deriving repr]

(* Shadow [Stdlib.unit] *)
module Nobuiltin_t = struct
  type t = (unit[@nobuiltin]) [@@deriving repr]

  (* [t]'s repr should be for strings. *)
  let (_ : string Repr.t) = t
end

module Nobuiltin_foo = struct
  type foo = (unit[@repr.nobuiltin]) [@@deriving repr]

  (* [foo]'s repr should be for strings too. *)
  let (_ : string Repr.t) = foo_t
end

module Nobuiltin_operator = struct
  (* Define our own representation of [result]. *)
  let result_t a b = Repr.pair a b
  let int32_t = Repr.int
  let int64_t = Repr.bool

  type u = (((int32[@nobuiltin]), int64) result[@nobuiltin]) [@@deriving repr]

  let (_ : (int * int64) Repr.t) = u_t
end
