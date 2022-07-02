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

let name = "ppx_repr"

module Unsupported = struct
  let tuple_size ~loc count =
    Location.raise_errorf ~loc
      "%s: tuple types must have 2 or 3 components. Found %d." name count

  let type_arrow ~loc ctyp =
    Location.raise_errorf ~loc
      "%s: function type encountered: %a. Functions are not Irmin-serialisable."
      name Pprintast.core_type ctyp

  let type_open ~loc =
    Location.raise_errorf ~loc
      "%s: extensible variant types are not Irmin-serialisable." name

  let type_poly ~loc ctyp =
    Location.raise_errorf ~loc
      "%s: universally-quantified type %a encountered. Irmin types must be \
       grounded."
      name Pprintast.core_type ctyp

  let type_open_polyvar ~loc ctyp =
    Location.raise_errorf ~loc
      "%s: open polymorphic variant %a encountered. Polymorphic variants must \
       be closed."
      name Pprintast.core_type ctyp

  let polyvar_inherit_case ~loc ctyp =
    Location.raise_errorf ~loc
      "%s: inherited variant cases encountered in %a. This is unsupported by \
       ppx_repr."
      name Pprintast.core_type ctyp

  let type_package ~loc ctyp =
    Location.raise_errorf ~loc
      "%s: package type %a encountered. Package types are not \
       Irmin-serialisable."
      name Pprintast.core_type ctyp

  let type_extension ~loc ctyp =
    Location.raise_errorf ~loc "%s: unprocessed extension %a encountered." name
      Pprintast.core_type ctyp

  let type_alias ~loc ctyp =
    Location.raise_errorf ~loc
      "%s: alias type %a encountered. Alias types are not supported." name
      Pprintast.core_type ctyp

  let type_any ~loc =
    Location.raise_errorf ~loc "%s: anonymous type variable unsupported." name

  let plugin ~loc ~supported found =
    Location.raise_errorf ~loc
      "%s: unsupported deriver plugin %s. Registered plugins: %a" name found
      Fmt.(Dump.list string)
      supported

  let recursive_groups ~loc tys =
    Location.raise_errorf ~loc
      "%s: mutually-recursive groups of size > 2 are not supported. Here we \
       have %d types: %a)"
      name (List.length tys)
      Fmt.(list ~sep:(any ", ") string)
      tys

  let recursive_type_with_type_paramets ~loc ty =
    Location.raise_errorf ~loc
      "%s: Can't support mutually-recursive types with type parameters in type \
       %s"
      name ty
end
