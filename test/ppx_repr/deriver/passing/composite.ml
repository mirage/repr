(* Tests of the composite type combinators *)
type test_list1 = string list [@@deriving repr]

type test_list2 = int32 list list list [@@deriving repr]

type test_array = bool array [@@deriving repr]

type test_option = unit option [@@deriving repr]

type test_pair = string * int32 [@@deriving repr]

type test_triple = string * int32 * bool [@@deriving repr]

type test_result = (int32, string) result [@@deriving repr]

let (_ : test_list1 Repr.Type.t) = test_list1_t

let (_ : test_list2 Repr.Type.t) = test_list2_t

let (_ : test_array Repr.Type.t) = test_array_t

let (_ : test_option Repr.Type.t) = test_option_t

let (_ : test_pair Repr.Type.t) = test_pair_t

let (_ : test_triple Repr.Type.t) = test_triple_t

let (_ : test_result Repr.Type.t) = test_result_t
