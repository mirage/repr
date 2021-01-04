module type Type = sig
  include Type_combinators.DSL with type 'a t = 'a Type_combinators.t

  (** {1 Staging} *)

  type +'a staged
  (** The type for staged operations. *)

  val stage : 'a -> 'a staged
  (** [stage x] stages [x]. *)

  val unstage : 'a staged -> 'a
  (** [unstage x] unstages [x]. *)

  (** {1:generics Generic Operations}

      Given a value ['a t], it is possible to define generic operations on value
      of type ['a] such as pretty-printing, parsing and unparsing. *)

  type 'a equal = ('a -> 'a -> bool) staged

  val equal : 'a t -> 'a equal
  (** [equal t] is the equality function between values of type [t]. *)

  type 'a compare = ('a -> 'a -> int) staged

  val compare : 'a t -> 'a compare
  (** [compare t] compares values of type [t]. *)

  type 'a pp = 'a Fmt.t
  (** The type for pretty-printers. *)

  type 'a of_string = string -> ('a, [ `Msg of string ]) result
  (** The type for parsers. *)

  val pp : 'a t -> 'a pp
  (** [pp t] is the pretty-printer for values of type [t]. *)

  val pp_dump : 'a t -> 'a pp
  (** [pp_dump t] is the dump pretty-printer for values of type [t].

      This pretty-printer outputs an encoding which is as close as possible to
      native OCaml syntax, so that the result can easily be copy-pasted into an
      OCaml REPL to inspect the value further. *)

  val pp_ty : 'a t pp
  (** The pretty printer for generics of type {!t}. *)

  val to_string : 'a t -> 'a -> string
  (** [to_string t] is [Fmt.to_to_string (pp t)]. *)

  val of_string : 'a t -> 'a of_string
  (** [of_string t] parses values of type [t]. *)

  (** {2 JSON converters} *)

  module Json : sig
    (** Overlay on top of Jsonm to work with rewindable streams. *)

    type decoder
    (** The type for JSON decoder. *)

    val decoder : ?encoding:[< Jsonm.encoding ] -> [< Jsonm.src ] -> decoder
    (** Same as {!Jsonm.decoder}. *)

    val decode :
      decoder ->
      [> `Await | `End | `Error of Jsonm.error | `Lexeme of Jsonm.lexeme ]
    (** Same as {!Jsonm.decode}. *)

    val rewind : decoder -> Jsonm.lexeme -> unit
    (** [rewind d l] rewinds [l] on top of the current state of [d]. This allows
        to put back lexemes already seen. *)
  end

  type 'a encode_json = Jsonm.encoder -> 'a -> unit
  (** The type for JSON encoders. *)

  type 'a decode_json = Json.decoder -> ('a, [ `Msg of string ]) result
  (** The type for JSON decoders. *)

  val pp_json : ?minify:bool -> 'a t -> 'a Fmt.t
  (** Similar to {!dump} but pretty-prints the JSON representation instead of
      the OCaml one. See {!encode_json} for details about the encoding.

      For instance:

      {[
        type t = { foo : int option; bar : string list }

        let t =
          record "r" (fun foo bar -> { foo; bar })
          |+ field "foo" (option int) (fun t -> t.foo)
          |+ field "bar" (list string) (fun t -> t.bar)
          |> sealr

        let s = Fmt.strf "%a\n" (pp t) { foo = None; bar = [ "foo" ] }

        (* s is "{ foo = None; bar = [\"foo\"]; }" *)

        let j = Fmt.strf "%a\n" (pp_json t) { foo = None; bar = [ "foo" ] }

        (* j is "{ \"bar\":[\"foo\"] }" *)
      ]}

      {b NOTE:} this will automatically convert JSON fragments to valid JSON
      objects by adding an enclosing array if necessary. *)

  val encode_json : 'a t -> Jsonm.encoder -> 'a -> unit
  (** [encode_json t e] encodes [t] into the
      {{:http://erratique.ch/software/jsonm} jsonm} encoder [e]. The encoding is
      a relatively straightforward translation of the OCaml structure into JSON.
      The main highlights are:

      - The unit value [()] is translated into the empty object [{}].
      - OCaml ints are translated into JSON floats.
      - OCaml strings are translated into JSON strings. You must then ensure
        that the OCaml strings contains only valid UTF-8 characters.
      - OCaml options are translated differently depending on context: record
        fields with a value of [None] are removed from the JSON object; record
        fields with a value of [Some x] are automatically unboxed into x; and
        outside of records, [None] is translated into [null] and [Some x] into
        [{"some": x'}] with [x'] the JSON encoding of [x].
      - Variant cases built using {!case0} are represented as strings.
      - Variant cases built using {!case1} are represented as a record with one
        field; the field name is the name of the variant.

      {b NOTE:} this can be used to encode JSON fragments. It's the
      responsibility of the caller to ensure that the encoded JSON fragment fits
      properly into a well-formed JSON object. *)

  val decode_json : 'a t -> Jsonm.decoder -> ('a, [ `Msg of string ]) result
  (** [decode_json t e] decodes values of type [t] from the
      {{:http://erratique.ch/software/jsonm} jsonm} decoder [e]. *)

  val decode_json_lexemes :
    'a t -> Jsonm.lexeme list -> ('a, [ `Msg of string ]) result
  (** [decode_json_lexemes] is similar to {!decode_json} but uses an already
      decoded list of JSON lexemes instead of a decoder. *)

  val to_json_string : ?minify:bool -> 'a t -> 'a -> string
  (** [to_json_string] is {!encode_json} with a string encoder. *)

  val of_json_string : 'a t -> string -> ('a, [ `Msg of string ]) result
  (** [of_json_string] is {!decode_json} with a string decoder .*)

  (** {2 Binary Converters} *)

  type 'a encode_bin = ('a -> (string -> unit) -> unit) staged
  (** The type for binary encoders. *)

  type 'a decode_bin = (string -> int -> int * 'a) staged
  (** The type for binary decoders. *)

  type 'a size_of = ('a -> int option) staged
  (** The type for size function related to binary encoder/decoders. *)

  type 'a short_hash := (?seed:int -> 'a -> int) staged

  val short_hash : 'a t -> 'a short_hash
  (** [hash t x] is a short hash of [x] of type [t]. *)

  val pre_hash : 'a t -> 'a encode_bin
  (** [pre_hash t x] is the string representation of [x], of type [t], which
      will be used to compute the digest of the value. By default it's
      [to_bin_string t x] but it can be overriden by {!v}, {!like} and {!map}
      operators. *)

  val encode_bin : 'a t -> 'a encode_bin
  (** [encode_bin t] is the binary encoder for values of type [t]. *)

  val decode_bin : 'a t -> 'a decode_bin
  (** [decode_bin t] is the binary decoder for values of type [t]. *)

  val to_bin_string : 'a t -> ('a -> string) staged
  (** [to_bin_string t x] use {!encode_bin} to convert [x], of type [t], to a
      string.

      {b NOTE:} When [t] is {!Type.string} or {!Type.bytes}, the original buffer
      [x] is not prefixed by its size as {!encode_bin} would do. If [t] is
      {!Type.string}, the result is [x] (without copy). *)

  val of_bin_string : 'a t -> (string -> ('a, [ `Msg of string ]) result) staged
  (** [of_bin_string t s] is [v] such that [s = to_bin_string t v].

      {b NOTE:} When [t] is {!Type.string}, the result is [s] (without copy). *)

  val size_of : 'a t -> 'a size_of
  (** [size_of t x] is either the size of [encode_bin t x] or the binary
      encoding of [x], if the backend is not able to pre-compute serialisation
      lengths. *)

  module Unboxed : sig
    (** Unboxed operations assumes that value being serialized is fully filling
        the underlying buffer. When that's the case, it is not necessary to
        prefix the value's binary representation by its size, as it is exactly
        the buffer's size.

        Unboxed operations only apply to top-level string-like values. These are
        defined as follows:

        - they are not not embedded in a larger structured values;
        - they are either of type {!string} or {!bytes};
        - or they are built by combining {!like} and {!map} operators to
          top-level string-like values.

        When unboxed operations are applied to values not supporting that
        operation, they automatically fall-back to their boxed counter-part. *)

    val encode_bin : 'a t -> 'a encode_bin
    (** Same as {!encode_bin} for unboxed values. *)

    val decode_bin : 'a t -> 'a decode_bin
    (** Same as {!decode_bin} for unboxed values. *)

    val size_of : 'a t -> 'a size_of
    (** Same as {!size_of} for unboxed values. *)
  end

  module Binary_shape : sig
    include module type of Type_binary.Shape
    (** @inline *)
  end

  (** {1 Custom converters} *)

  type 'a ty = 'a t

  module Uuid : sig
    include module type of Type_binary.Uuid
    (** @inline *)
  end

  val v :
    pp:'a pp ->
    of_string:'a of_string ->
    json:'a encode_json * 'a decode_json ->
    bin:'a encode_bin * 'a decode_bin * 'a size_of ->
    ?unboxed_bin:'a encode_bin * 'a decode_bin * 'a size_of ->
    ?bin_codec_uuid:Uuid.t ->
    equal:'a equal ->
    compare:'a compare ->
    short_hash:'a short_hash ->
    pre_hash:'a encode_bin ->
    unit ->
    'a t

  val like :
    ?pp:'a pp ->
    ?of_string:'a of_string ->
    ?json:'a encode_json * 'a decode_json ->
    ?bin:'a encode_bin * 'a decode_bin * 'a size_of ->
    ?unboxed_bin:'a encode_bin * 'a decode_bin * 'a size_of ->
    ?bin_codec_uuid:Uuid.t ->
    ?equal:'a equal ->
    ?compare:'a compare ->
    ?short_hash:'a short_hash ->
    ?pre_hash:'a encode_bin ->
    'a t ->
    'a t

  val map :
    ?pp:'a pp ->
    ?of_string:'a of_string ->
    ?json:'a encode_json * 'a decode_json ->
    ?bin:'a encode_bin * 'a decode_bin * 'a size_of ->
    ?unboxed_bin:'a encode_bin * 'a decode_bin * 'a size_of ->
    ?bin_codec_uuid:Uuid.t ->
    ?equal:'a equal ->
    ?compare:'a compare ->
    ?short_hash:'a short_hash ->
    ?pre_hash:'a encode_bin ->
    ?uuid:Uuid.t ->
    'b t ->
    ('b -> 'a) ->
    ('a -> 'b) ->
    'a t

  module type S = sig
    type t

    val t : t ty
  end

  module type DSL = Type_combinators.DSL
end
