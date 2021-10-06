open Higher

(** An attribute key is a value that can be used to attach polymorphic data to a
    heterogeneous attribute map. *)
module type S1 = sig
  type 'a attr
  type 'a map

  type 'a t
  (** The type of data associated with the {!attr} attribute key. *)

  val add : 'a t -> 'a map -> 'a map
  (** Attach data for {!attr} to a given map. *)

  val find : 'a map -> 'a t option
  (** Search for data corresponding to the key {!attr} in the given map. *)

  include Branded.S with type 'a t := 'a t

  val key : br attr
end

module type Attribute = sig
  type 'f t
  (** An ['f t] is an attribute key that can be used to pack polymorphic data
      into a heterogeneous {!Map} (and then recover it again).

      The type parameter ['f] is the brand of a type operator [f : * ⇒ *] which,
      when applied to the type parameter ['a] of a {!Map.t}, gives the type
      ['a f] of the associated data. This allows a single attribute key to store
      {i polymorphic} data. *)

  val create : name:string -> _ t
  (** [create ~name] is a fresh attribute key with the given string name. *)

  val name : _ t -> string
  (** Get the string name of an attribute key. *)

  module Map : sig
    type 'f key := 'f t

    type 'a t
    (** The type of polymorphic, heterogeneous maps. *)

    type ('a, 'f) data := ('a, 'f) app
    (** Given an ['a t] map and an ['f key] attribute key, the type of the
        corresponding data is [('a, 'f) Higher.app]. *)

    val empty : _ t
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
