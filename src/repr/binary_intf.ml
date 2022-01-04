open Type_core
open Staging

module Types = struct
  type 'a encoder = 'a -> (string -> unit) -> unit
  type 'a decoder = string -> int ref -> 'a
  type 'a sizer = 'a Size.Sizer.t
end

open Types

module type S = sig
  type t

  val encode : t encoder
  val decode : t decoder
  val sizer : t sizer
end

module type S_with_length = sig
  type t

  val encode : len -> t encoder staged
  val decode : len -> t decoder staged
  val sizer : len -> t sizer
end

module type S1 = sig
  type 'a t

  val encode : 'a encoder -> 'a t encoder
  val decode : 'a decoder -> 'a t decoder
  val sizer : 'a sizer -> 'a t sizer
end

module type S1_with_length = sig
  type 'a t

  val encode : len -> 'a encoder -> 'a t encoder staged
  val decode : len -> 'a decoder -> 'a t decoder staged
  val sizer : len -> 'a sizer -> 'a t sizer
end

module type S2 = sig
  type ('a, 'b) t

  val encode : 'a encoder -> 'b encoder -> ('a, 'b) t encoder
  val decode : 'a decoder -> 'b decoder -> ('a, 'b) t decoder
  val sizer : 'a sizer -> 'b sizer -> ('a, 'b) t sizer
end

module type S3 = sig
  type ('a, 'b, 'c) t

  val encode : 'a encoder -> 'b encoder -> 'c encoder -> ('a, 'b, 'c) t encoder
  val decode : 'a decoder -> 'b decoder -> 'c decoder -> ('a, 'b, 'c) t decoder
  val sizer : 'a sizer -> 'b sizer -> 'c sizer -> ('a, 'b, 'c) t sizer
end

module type Intf = sig
  include module type of Types

  module type S = S
  module type S1 = S1
  module type S2 = S2
  module type S3 = S3

  module Unit : S with type t := unit
  module Bool : S with type t := bool
  module Char : S with type t := char
  module Varint : S with type t := int
  module Varint_int63 : S with type t := Optint.Int63.t
  module Int16 : S with type t := int
  module Int32 : S with type t := int32
  module Int64 : S with type t := int64
  module Float : S with type t := float
  module String : S_with_length with type t := string
  module String_unboxed : S_with_length with type t := string
  module Bytes : S_with_length with type t := bytes
  module Bytes_unboxed : S_with_length with type t := bytes
  module List : S1_with_length with type 'a t := 'a list
  module Array : S1_with_length with type 'a t := 'a array
  module Option : S1 with type 'a t := 'a option
  module Pair : S2 with type ('a, 'b) t := 'a * 'b
  module Triple : S3 with type ('a, 'b, 'c) t := 'a * 'b * 'c
end
