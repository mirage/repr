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

module Encode : sig
  val t : 'a t -> 'a encode_bin staged
  val unboxed : 'a t -> 'a encode_bin staged

  module Attr : Attribute.S1 with type 'a t = 'a encode_bin
  module Attr_unboxed : Attribute.S1 with type 'a t = 'a encode_bin
end

module Decode : sig
  val t : 'a t -> 'a decode_bin staged
  val unboxed : 'a t -> 'a decode_bin staged

  module Attr : Attribute.S1 with type 'a t = 'a decode_bin
  module Attr_unboxed : Attribute.S1 with type 'a t = 'a decode_bin
end

module Pre_hash : sig
  val t : 'a t -> 'a encode_bin staged

  module Attr : Attribute.S1 with type 'a t = 'a encode_bin
end

module Short_hash : sig
  val t : 'a t -> 'a short_hash staged

  module Attr : Attribute.S1 with type 'a t = 'a short_hash
end

val to_bin_string : 'a t -> 'a to_string staged
val of_bin_string : 'a t -> 'a of_string staged
