(*
 * Copyright (c) 2016-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Type_core
open Staging

module Uuid : sig
  type t = string

  val t : t ty
  val of_string : string -> t
end

module Shape : sig
  type t
  (** The type of {i shapes} of a particular type with respect to the binary
      decoders defined here. Shapes are represented canonically such that
      equality of shapes of two types [t1] and [t2] implies equivalence of the
      binary codecs derived from [t1] and [t2].

      This can be used to ensure that changes to a type representation do not
      alter the binary representation of values of that type. *)

  val t : t ty
  val of_type : _ ty -> t
end

val encode_bin : 'a t -> 'a encode_bin
val decode_bin : 'a t -> 'a decode_bin

module Unboxed : sig
  val encode_bin : 'a t -> 'a encode_bin
  val decode_bin : 'a t -> 'a decode_bin
end

val to_bin_string : 'a t -> 'a to_string staged
val of_bin_string : 'a t -> 'a of_string staged
