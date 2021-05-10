open Type_core
open Staging

module type IO_channel = sig
  type out_channel

  val append_char : out_channel -> char -> unit
  val append_string : out_channel -> string -> unit
  val append_bytes : out_channel -> bytes -> unit
  val append_byte : out_channel -> int -> unit

  type in_channel

  val input_byte : in_channel -> int -> int
  (** [input_byte ic off] is [byte] *)

  val input_char : in_channel -> int -> char
  (** [input_char ic off] is [char] *)

  val blit : in_channel -> int -> bytes -> int -> int -> unit
end

module type S = sig
  type out_channel
  type in_channel
  type 'a t
  type +'a staged
  type 'a bin_seq = 'a -> out_channel -> unit
  type 'a encode_bin = 'a bin_seq staged
  type 'a decode_bin = (in_channel -> int -> int * 'a) staged

  val encode_bin : 'a t -> 'a encode_bin
  (** [encode_bin t] is the binary encoder for values of type [t]. *)

  val decode_bin : 'a t -> 'a decode_bin
  (** [decode_bin t] is the binary decoder for values of type [t]. *)

  val pre_hash : 'a t -> 'a encode_bin
  (** [pre_hash t x] is the string representation of [x], of type [t], which
      will be used to compute the digest of the value. By default it's
      [to_bin_string t x] but it can be overriden by {!v}, {!like} and {!map}
      operators. *)

  module Unboxed : sig
    val encode_bin : 'a t -> 'a encode_bin
    val decode_bin : 'a t -> 'a decode_bin
  end
end

module type Maker = functor (IO : IO_channel) ->
  S
    with type out_channel = IO.out_channel
     and type in_channel = IO.in_channel
     and type 'a t = 'a Type_core.t
     and type +'a staged = 'a Staging.staged

module type Sigs = sig
  module type IO_channel = IO_channel
  module type S = S
  module type Maker = Maker

  module Make : Maker

  val to_bin_string : 'a t -> 'a to_string staged
  (** [to_bin_string t x] use {!encode_bin} to convert [x], of type [t], to a
      string.

      {b NOTE:} When [t] is {!Type.string} or {!Type.bytes}, the original buffer
      [x] is not prefixed by its size as {!encode_bin} would do. If [t] is
      {!Type.string}, the result is [x] (without copy). *)

  val of_bin_string : 'a t -> 'a of_string staged
  (** [of_bin_string t s] is [v] such that [s = to_bin_string t v].

      {b NOTE:} When [t] is {!Type.string}, the result is [s] (without copy). *)

  type 'a short_hash := (?seed:int -> 'a -> int) staged

  val short_hash : 'a t -> 'a short_hash
  (** [hash t x] is a short hash of [x] of type [t]. *)

end
