(* Tests of type aliases *)
type test_result = (int, string) result [@@deriving repr]
type t_alias = test_result [@@deriving repr]
type t = t_alias [@@deriving repr]

let (_ : test_result Repr.t) = test_result_t
let (_ : t_alias Repr.t) = t_alias_t
let (_ : t Repr.t) = t
