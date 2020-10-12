(* Ensure that the [Json] module in [Repr] doesn't shadow references to
   types contained in a different [Json] module.

   Regression test for https://github.com/mirage/irmin/issues/923. *)

module Json = struct
  type t = string

  let t = Repr.string
end

type foo = { contents : Json.t } [@@deriving repr]

let (_ : foo Repr.t) = foo_t
