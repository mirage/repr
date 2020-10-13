(* Tests of the composite type combinators *)
type test_list1 = string list [@@deriving repr]
type test_list2 = int32 list list list [@@deriving repr]
type test_array = bool array [@@deriving repr]
type test_option = unit option [@@deriving repr]
type test_pair = string * int32 [@@deriving repr]
type test_triple = string * int32 * bool [@@deriving repr]
type test_result = (int32, string) result [@@deriving repr]

let (_ : test_list1 Repr.t) = test_list1_t
let (_ : test_list2 Repr.t) = test_list2_t
let (_ : test_array Repr.t) = test_array_t
let (_ : test_option Repr.t) = test_option_t
let (_ : test_pair Repr.t) = test_pair_t
let (_ : test_triple Repr.t) = test_triple_t
let (_ : test_result Repr.t) = test_result_t
