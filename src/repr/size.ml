type 'a t = Static of int | Dynamic of 'a | Unknown
type 'a size = 'a t

let map : type a b. (a -> b) -> a t -> b t =
 fun f -> function
  | Unknown -> Unknown
  | Static n -> Static n
  | Dynamic a -> Dynamic (f a)

module Syntax = struct
  let ( let+ ) x f = map f x
end

(** A type wrapper for positional offsets into buffers (as opposed to e.g.
    lengths of values in those buffers). *)
type offset = Offset of int [@@unboxed]

module Offset = struct
  type t = offset

  let ( +> ) : t -> int -> t = fun (Offset n) m -> Offset (n + m)
  let ( <+ ) : int -> t -> t = fun n (Offset m) -> Offset (n + m)
end

module Sizer = struct
  type 'a t = {
    of_value : ('a -> int) size;
    of_encoding : (string -> Offset.t -> Offset.t) size;
  }
  (** An ['a t] is a value that represents the size information known about a
      particular codec for type ['a].

      - [of_value]: given a value to encode, return the size of its encoding.

      - [of_encoding]: given a buffer [buf] and an offset [off], return the
        _offset_ immediately _after_ the encoding starting at [buf.\[off\]]
        NOTE: not the length of the encoding itself, to enable chains of such
        sizers to call each other in tail-position.

      Invariant: [∀ n. (of_value = Static n) ⟺ (of_encoding = Static n)]. *)

  let ( <+> ) : type a. a t -> a t -> a t =
    let add_of_value (a : _ size) (b : _ size) : _ size =
      match (a, b) with
      | Unknown, _ | _, Unknown -> Unknown
      | Static a, Static b -> Static (a + b)
      | Static 0, other | other, Static 0 -> other
      | Static n, Dynamic f | Dynamic f, Static n -> Dynamic (fun a -> n + f a)
      | Dynamic f, Dynamic g -> Dynamic (fun a -> f a + g a)
    in
    let add_of_encoding (a : _ size) (b : _ size) : _ size =
      match (a, b) with
      | Unknown, _ | _, Unknown -> Unknown
      | Static a, Static b -> Static (a + b)
      | Static 0, other | other, Static 0 -> other
      | Dynamic f, Dynamic g -> Dynamic (fun buf off -> g buf (f buf off))
      (* NOTE: in these cases we could be slightly more efficient by storing a
         vector of sizing functions inside [Dynamic], which would allow constant
         folding for static segments of dynamically-sized types. *)
      | Static n, Dynamic f -> Dynamic (fun buf off -> f buf Offset.(off +> n))
      | Dynamic f, Static n -> Dynamic (fun buf off -> Offset.(f buf off +> n))
    in
    fun a b ->
      {
        of_value = add_of_value a.of_value b.of_value;
        of_encoding = add_of_encoding a.of_encoding b.of_encoding;
      }

  let static n = { of_value = Static n; of_encoding = Static n }

  let dynamic ~of_value ~of_encoding =
    { of_value = Dynamic of_value; of_encoding = Dynamic of_encoding }

  let using f t =
    let of_value = map (fun size_of x -> size_of (f x)) t.of_value in
    { t with of_value }

  let unknown = { of_value = Unknown; of_encoding = Unknown }
end
