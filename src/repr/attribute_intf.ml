open Brands

module type S1 = sig
  type 'a attr
  type 'a map
  type 'a t

  include Branded.S1 with type 'a t := 'a t

  val attr : br attr
  val find_attr : 'a map -> 'a t option
end

module type Attribute = sig
  type 'f t

  val create : name:string -> _ t
  val name : _ t -> string

  module Map : sig
    type 'f key := 'f t
    type 'a t
    type ('a, 'f) data := ('a, 'f) app

    val empty : unit -> _ t
    val is_empty : _ t -> bool
    val mem : 'a t -> 'f key -> bool
    val add : 'a t -> key:'f key -> data:('a, 'f) data -> 'a t

    val update :
      'a t -> 'f key -> (('a, 'f) data option -> ('a, 'f) data option) -> 'a t

    val singleton : 'f key -> ('a, 'f) data -> 'a t

    type 'a binding = B : 'f key * ('a, 'f) data -> 'a binding

    val iter : 'a t -> f:('a binding -> unit) -> unit
    val for_all : 'a t -> f:('a binding -> bool) -> bool
    val exists : 'a t -> f:('a binding -> bool) -> bool
    val cardinal : 'a t -> int
    val find : 'a t -> 'f key -> ('a, 'f) data option
    val bindings : 'a t -> 'a binding list
  end

  module type S1 = S1 with type 'a attr := 'a t and type 'a map := 'a Map.t

  module Make1 (T : sig
    type 'a t

    val name : string
  end) : S1 with type 'a t = 'a T.t
end
