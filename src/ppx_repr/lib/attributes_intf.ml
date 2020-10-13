open Ppxlib

module type S = sig
  val repr : (core_type, expression) Attribute.t
  val nobuiltin : (core_type, unit) Attribute.t
end

module type Attributes = sig
  module type S = S

  module Make (T : sig
    val namespace : string
  end) : sig
    include S

    val all : Attribute.packed list
    (** Boxed list of all of the attributes required by [ppx_repr]. *)
  end
end
