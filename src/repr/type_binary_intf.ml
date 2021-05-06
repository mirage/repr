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
  type 'a bin_seq = 'a -> out_channel -> unit
  type 'a encode_bin = 'a bin_seq staged
  type 'a decode_bin = (in_channel -> int -> int * 'a) staged

  val encode_bin : 'a t -> 'a encode_bin
  val decode_bin : 'a t -> 'a decode_bin

  module Unboxed : sig
    val encode_bin : 'a t -> 'a encode_bin
    val decode_bin : 'a t -> 'a decode_bin
  end
end

module type Maker = sig
  module Make (IO : IO_channel) : sig
    include
      S
        with type out_channel = IO.out_channel
         and type in_channel = IO.in_channel
  end
end

module type Sigs = sig
  module type IO_channel = IO_channel
  module type S = S
  module type Maker = Maker
end
