(*
 * Copyright (c) 2019-2020 Craig Ferguson <me@craigfe.io>
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

open Ppxlib

(** This module defines the expected combinators exposed by Repr, and their
    correspondences to the types in the standard library. *)

let rec drop_stdlib_prefix = function
  | (Lident _ | Lapply _) as l -> l
  | Ldot (Lident "Stdlib", suffix) -> Lident suffix
  | Ldot (l, suffix) -> Ldot (drop_stdlib_prefix l, suffix)

let basic =
  [
    "unit";
    "bool";
    "char";
    "int";
    "int32";
    "int63";
    "int64";
    "float";
    "string";
    "bytes";
    "list";
    "array";
    "option";
    "result";
  ]

let containers = [ "either"; "hashtbl"; "queue"; "seq"; "stack" ]
let type_in_default_scope name = (Lident name, name)

let type_in_separate_module name =
  (Ldot (Lident (String.capitalize_ascii name), "t"), name)

let type_to_combinator_name : longident -> string option =
  let correspondences =
    let assoc =
      (* Abstract types with equivalent combinator names: *)
      List.map type_in_default_scope basic
      (* Types named [t] within their own modules: *)
      @ List.map type_in_separate_module (basic @ containers)
      (* Technically [lazy_t] is not for direct use, but derive anyway: *)
      @ [ (Lident "lazy_t", "lazy_t"); (Ldot (Lident "Lazy", "t"), "lazy_t") ]
      (* [Int63.t] may be namespaced under [Optint.Int63.t]: *)
      @ [ (Ldot (Ldot (Lident "Optint", "Int63"), "t"), "int63") ]
    in
    List.to_seq assoc |> Hashtbl.of_seq
  in
  fun lident -> Hashtbl.find_opt correspondences (drop_stdlib_prefix lident)
