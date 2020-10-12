(* Nested tuple type *)
type deep_tuple =
  (((int32 * int32) * int32 * int32) * int32 * int32) * int32 * int32
[@@deriving repr]

let (_ : deep_tuple Repr.Type.t) = deep_tuple_t
