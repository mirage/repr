open Ppxlib

(** A [Plugin.t] is a pair of functions that extend a representable type with
    specialised generic operations: one to supply the implementation of the
    specialistion, and one to supply its type.

    For instance, the generic operation [equal : 'a Repr.t -> 'a -> 'a -> equal]
    could be packaged as the following plugin:

    {[
      create
        ~intf:(fun loc t -> [%type: [%t t] -> [%t t] -> bool])
        ~impl:(fun loc t -> [%expr Repr.unstage (Repr.equal [%e t])])
    ]}

    That is:

    - given some type [t], its equality function has type [t -> t -> bool],
    - given a runtime representation of [t], we can derive an equality function
      via [Repr.equal]. *)
module Plugin : sig
  type t

  val create :
    ?type_name:[ `before | `after ]
      (** Position of the type name relative to the operation name in the
          derived value (i.e. [`before] ↦ [val date_random], and [`after] ↦
          [val random_date]). *) ->
    impl:(location -> expression -> expression) ->
    intf:(location -> core_type -> core_type) ->
    string ->
    t

  val derive_str :
    t ->
    loc:location ->
    type_name:string ->
    params:string list ->
    expr:expression ->
    structure_item

  val derive_sig :
    t ->
    loc:location ->
    type_name:string ->
    params:core_type list ->
    ctyp:core_type ->
    signature_item

  val defaults : t list
  (** Default set of plugins, using the generic operations provided by {!Repr}. *)
end

module Args : sig
  type (_, _) t =
    | [] : ('a, 'a) t
    | ( :: ) : 'a Deriving.Args.param * ('b, 'c) t -> ('a -> 'b, 'c) t
end

val make_generator :
  ?attributes:Ppxlib.Attribute.packed list ->
  ?deps:Ppxlib.Deriving.t list ->
  args:('a, 'b) Args.t ->
  supported_plugins:Plugin.t list ->
  (loc:location -> path:string -> Plugin.t list -> 'c -> 'a) ->
  ('b, 'c) Ppxlib.Deriving.Generator.t
(** An extension of {!Ppxlib.Deriving.make_generator} that supports a set of
    meta-deriving plugins. *)
