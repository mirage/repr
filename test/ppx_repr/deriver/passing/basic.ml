type int63 = Optint.Int63.t

module type BASIC = sig
  val test_unit_t : unit Repr.t
  val test_bool_t : bool Repr.t
  val test_char_t : char Repr.t
  val test_int_t : int Repr.t
  val test_int32_t : int32 Repr.t
  val test_int63_t : int63 Repr.t
  val test_int64_t : int64 Repr.t
  val test_float_t : float Repr.t
  val test_string_t : string Repr.t
  val test_bytes_t : bytes Repr.t
end

module Basic : BASIC = struct
  type test_unit = unit [@@deriving repr]
  type test_bool = bool [@@deriving repr]
  type test_char = char [@@deriving repr]
  type test_int = int [@@deriving repr]
  type test_int32 = int32 [@@deriving repr]
  type test_int63 = int63 [@@deriving repr]
  type test_int64 = int64 [@@deriving repr]
  type test_float = float [@@deriving repr]
  type test_string = string [@@deriving repr]
  type test_bytes = bytes [@@deriving repr]
end

module Composite : sig end = struct
  type test_list1 = string list [@@deriving repr]
  type test_list2 = int32 list list list [@@deriving repr]
  type test_array = bool array [@@deriving repr]
  type test_option = unit option [@@deriving repr]
  type test_pair = string * int32 [@@deriving repr]
  type test_triple = string * int32 * bool [@@deriving repr]
  type test_result = (int32 lazy_t, string) result [@@deriving repr]

  let (_ : test_list1 Repr.t) = test_list1_t
  let (_ : test_list2 Repr.t) = test_list2_t
  let (_ : test_array Repr.t) = test_array_t
  let (_ : test_option Repr.t) = test_option_t
  let (_ : test_pair Repr.t) = test_pair_t
  let (_ : test_triple Repr.t) = test_triple_t
  let (_ : test_result Repr.t) = test_result_t
end

module Inside_modules : sig
  include BASIC

  (* Aliases of composite types *)
  val test_list_t : 'a Repr.t -> 'a List.t Repr.t
  val test_array_t : 'a Repr.t -> 'a Array.t Repr.t
  val test_option_t : 'a Repr.t -> 'a Option.t Repr.t
  val test_lazy_t : 'a Repr.t -> 'a Lazy.t Repr.t
  val test_result_t : 'a Repr.t -> 'b Repr.t -> ('a, 'b) Result.t Repr.t

  (* Other container types *)
  val test_seq_t : 'a Repr.t -> 'a Seq.t Repr.t
  val test_hashtbl_t : 'a Repr.t -> 'b Repr.t -> ('a, 'b) Hashtbl.t Repr.t
  val test_stack_t : 'a Repr.t -> 'a Stack.t Repr.t
  val test_queue_t : 'a Repr.t -> 'a Queue.t Repr.t
  val test_either_t : 'a Repr.t -> 'b Repr.t -> ('a, 'b) Either.t Repr.t
end = struct
  type test_unit = Unit.t [@@deriving repr]
  type test_bool = Bool.t [@@deriving repr]
  type test_char = Char.t [@@deriving repr]
  type test_int = Int.t [@@deriving repr]
  type test_int32 = Int32.t [@@deriving repr]
  type test_int63 = Optint.Int63.t [@@deriving repr]
  type test_int64 = Int64.t [@@deriving repr]
  type test_float = Float.t [@@deriving repr]
  type test_string = String.t [@@deriving repr]
  type test_bytes = Bytes.t [@@deriving repr]
  type 'a test_list = 'a List.t [@@deriving repr]
  type 'a test_array = 'a Array.t [@@deriving repr]
  type 'a test_option = 'a Option.t [@@deriving repr]
  type 'a test_lazy = 'a Lazy.t [@@deriving repr]
  type ('a, 'b) test_result = ('a, 'b) Result.t [@@deriving repr]
  type 'a test_seq = 'a Seq.t [@@deriving repr]
  type ('a, 'b) test_hashtbl = ('a, 'b) Hashtbl.t [@@deriving repr]
  type 'a test_stack = 'a Stack.t [@@deriving repr]
  type 'a test_queue = 'a Queue.t [@@deriving repr]
  type ('a, 'b) test_either = ('a, 'b) Either.t [@@deriving repr]
end
