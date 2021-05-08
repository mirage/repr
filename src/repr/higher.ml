(** Defunctionalised higher-kinded types. See "Lightweight Higher-Kinded
    Polymorphism" (Yallop and White, 2014) for more details. *)

type ('a, 'f) app

module Branded = struct
  module type S = sig
    type 'a t
    type br

    external inj : 'a t -> ('a, br) app = "%identity"
    external prj : ('a, br) app -> 'a t = "%identity"
  end

  module Make (T : sig
    type 'a t
  end) : S with type 'a t := 'a T.t = struct
    type 'a t = 'a T.t
    type br

    external inj : 'a t -> ('a, br) app = "%identity"
    external prj : ('a, br) app -> 'a t = "%identity"
  end
end
