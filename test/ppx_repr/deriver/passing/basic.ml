(* Tests of the basic types *)
type test_unit = unit [@@deriving repr]
type test_bool = bool [@@deriving repr]
type test_char = char [@@deriving repr]
type test_int32 = int32 [@@deriving repr]
type test_int64 = int64 [@@deriving repr]
type test_float = float [@@deriving repr]
type test_string = string [@@deriving repr]
