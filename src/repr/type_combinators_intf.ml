module type DSL = sig
  (** {1 Type Combinators} *)

  type 'a t
  (** The type for runtime representation of values of type ['a]. *)

  type len = [ `Int | `Int8 | `Int16 | `Int32 | `Int64 | `Fixed of int ]
  (** The type of integer used to store buffers, list or array lengths.

      [Int] use a (compressed) variable encoding to encode integers in a binary
      format, while [IntX] always use [X] bytes. Overflows are not detected. *)

  (** {1:primitives Primitives} *)

  val unit : unit t
  (** [unit] is a representation of the unit type. *)

  val bool : bool t
  (** [bool] is a representation of the boolean type. *)

  val char : char t
  (** [char] is a representation of the character type. *)

  val int : int t
  (** [int] is a representation of integers. Binary serialization uses a
      varying-width representation. *)

  val int32 : int32 t
  (** [int32] is a representation of the 32-bit integer type. *)

  val int64 : int64 t
  (** [int64] is a representation of the 64-bit integer type. *)

  val float : float t
  (** [float] is a representation of the [float] type. *)

  val string : string t
  (** [string] is a representation of the [string] type. *)

  val bytes : bytes t
  (** [bytes] is a representation of the [bytes] type. *)

  val string_of : len -> string t
  (** Like {!string} but with a given kind of size. *)

  val bytes_of : len -> bytes t
  (** Like {!bytes} but with a given kind of size. *)

  val boxed : 'a t -> 'a t
  (** [boxed t] is the same as [t] but with a binary representation which is
      always boxed (e.g. top-level values won't be unboxed). This forces
      {!Unboxed} functions to be exactly the same as boxed ones.*)

  val list : ?len:len -> 'a t -> 'a list t
  (** [list t] is a representation of lists of values of type [t]. *)

  val array : ?len:len -> 'a t -> 'a array t
  (** [array t] is a representation of arrays of values of type [t]. *)

  val option : 'a t -> 'a option t
  (** [option t] is a representation of values of type [t option]. *)

  val pair : 'a t -> 'b t -> ('a * 'b) t
  (** [pair x y] is a representation of values of type [x * y]. *)

  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  (** [triple x y z] is a representation of values of type [x * y * z]. *)

  val result : 'a t -> 'b t -> ('a, 'b) result t
  (** [result a b] is a representation of values of type [(a, b) result]. *)

  (** An uninhabited type, defined as a variant with no constructors. *)
  type empty = |

  val empty : empty t
  (** [empty] is a representation of the {!empty} type. *)

  (** {1:records Records} *)

  type ('a, 'b, 'c) open_record
  (** The type for representing open records of type ['a] with a constructor of
      type ['b]. ['c] represents the remaining fields to be described using the
      {!(|+)} operator. An open record initially satisfies ['c = 'b] and can be
      {{!sealr} sealed} once ['c = 'a]. *)

  val record : string -> 'b -> ('a, 'b, 'b) open_record
  (** [record n f] is an incomplete representation of the record called [n] of
      type ['a] with constructor [f]. To complete the representation, add fields
      with {!(|+)} and then seal the record with {!sealr}. *)

  type ('a, 'b) field
  (** The type for fields holding values of type ['b] and belonging to a record
      of type ['a]. *)

  val field : string -> 'a t -> ('b -> 'a) -> ('b, 'a) field
  (** [field n t g] is the representation of the field [n] of type [t] with
      getter [g]. {b Raises.} [Invalid_argument] if [n] is not valid UTF-8.

      The name [n] must not be used by any other [field] in the record.

      For instance:

      {[
        type manuscript = { title : string option }

        let manuscript = field "title" (option string) (fun t -> t.title)
      ]} *)

  val ( |+ ) :
    ('a, 'b, 'c -> 'd) open_record -> ('a, 'c) field -> ('a, 'b, 'd) open_record
  (** [r |+ f] is the open record [r] augmented with the field [f]. *)

  val sealr : ('a, 'b, 'a) open_record -> 'a t
  (** [sealr r] seals the open record [r]. {b Raises.} [Invalid_argument] if two
      or more fields share the same name. *)

  (** Putting all together:

      {[
        type menu = { restaurant : string; items : (string * int32) list }

        let t =
          record "t" (fun restaurant items -> { restaurant; items })
          |+ field "restaurant" string (fun t -> t.restaurant)
          |+ field "items" (list (pair string int32)) (fun t -> t.items)
          |> sealr
      ]} *)

  (** {1:variants Variants} *)

  type ('a, 'b, 'c) open_variant
  (** The type for representing open variants of type ['a] with pattern matching
      of type ['b]. ['c] represents the remaining constructors to be described
      using the {!(|~)} operator. An open variant initially satisfies [c' = 'b]
      and can be {{!sealv} sealed} once ['c = 'a]. *)

  val variant : string -> 'b -> ('a, 'b, 'b) open_variant
  (** [variant n p] is an incomplete representation of the variant type called
      [n] of type ['a] using [p] to deconstruct values. To complete the
      representation, add cases with {!(|~)} and then seal the variant with
      {!sealv}. *)

  type ('a, 'b) case
  (** The type for representing variant cases of type ['a] with patterns of type
      ['b]. *)

  type 'a case_p
  (** The type for representing patterns for a variant of type ['a]. *)

  val case0 : string -> 'a -> ('a, 'a case_p) case
  (** [case0 n v] is a representation of a variant constructor [v] with no
      arguments and name [n]. {b Raises.} [Invalid_argument] if [n] is not valid
      UTF-8.

      The name [n] must not by used by any other [case0] in the record.

      For instance:

      {[
        type t = Foo

        let foo = case0 "Foo" Foo
      ]} *)

  val case1 : string -> 'b t -> ('b -> 'a) -> ('a, 'b -> 'a case_p) case
  (** [case1 n t c] is a representation of a variant constructor [c] with an
      argument of type [t] and name [n]. {b Raises.} [Invalid_argument] if [n]
      is not valid UTF-8.

      The name [n] must not by used by any other [case1] in the record.

      For instance:

      {[
        type t = Foo of string

        let foo = case1 "Foo" string (fun s -> Foo s)
      ]} *)

  val ( |~ ) :
    ('a, 'b, 'c -> 'd) open_variant ->
    ('a, 'c) case ->
    ('a, 'b, 'd) open_variant
  (** [v |~ c] is the open variant [v] augmented with the case [c]. *)

  val sealv : ('a, 'b, 'a -> 'a case_p) open_variant -> 'a t
  (** [sealv v] seals the open variant [v]. {b Raises.} [Invalid_argument] if
      two or more cases of same arity share the same name. *)

  (** Putting all together:

      {[
        type t = Foo | Bar of string

        let t =
          variant "t" (fun foo bar -> function Foo -> foo | Bar s -> bar s)
          |~ case0 "Foo" Foo
          |~ case1 "Bar" string (fun x -> Bar x)
          |> sealv
      ]} *)

  val enum : string -> (string * 'a) list -> 'a t
  (** [enum n cs] is a representation of the variant type called [n] with
      singleton cases [cs]. e.g.

      {[
        type t = Foo | Bar | Toto

        let t = enum "t" [ ("Foo", Foo); ("Bar", Bar); ("Toto", Toto) ]
      ]}

      {b Raises.} [Invalid_argument] if two or more cases share the same name. *)

  (** {1:recursive Recursive definitions}

      [Type] allows a limited description of recursive records and variants.

      {b TODO}: describe the limitations, e.g. only regular recursion and no use
      of the generics inside the [mu*] functions and the usual caveats with
      recursive values (such as infinite loops on most of the generics which
      don't check sharing). *)

  val mu : ('a t -> 'a t) -> 'a t
  (** [mu f] is the representation [r] such that [r = mu r].

      For instance:

      {[
        type x = { x : x option }

        let x =
          mu (fun x ->
              record "x" (fun x -> { x })
              |+ field "x" (option x) (fun x -> x.x)
              |> sealr)
      ]} *)

  val mu2 : ('a t -> 'b t -> 'a t * 'b t) -> 'a t * 'b t
  (** [mu2 f] is the representations [r] and [s] such that [r, s = mu2 r s].

      For instance:

      {[
        type r = { foo : int; bar : string list; z : z option }

        and z = { x : int; r : r list }

        (* Build the representation of [r] knowing [z]'s. *)
        let mkr z =
          record "r" (fun foo bar z -> { foo; bar; z })
          |+ field "foo" int (fun t -> t.foo)
          |+ field "bar" (list string) (fun t -> t.bar)
          |+ field "z" (option z) (fun t -> t.z)
          |> sealr

        (* And the representation of [z] knowing [r]'s. *)
        let mkz r =
          record "z" (fun x r -> { x; r })
          |+ field "x" int (fun t -> t.x)
          |+ field "r" (list r) (fun t -> t.r)
          |> sealr

        (* Tie the loop. *)
        let r, z = mu2 (fun r z -> (mkr z, mkz y))
      ]} *)
end

module type Type_combinators = sig
  module type DSL = DSL

  include DSL with type 'a t = 'a Type_core.t
  (** @inline *)
end
