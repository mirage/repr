type 'a typ = 'a Repr.t

module Id : sig
  type 'a t [@@deriving repr]
end = struct
  type 'a t = 'a [@@deriving repr]
end

let __ : type a. a typ -> a Id.t typ = Id.t

module Phantom : sig
  type _ t = int [@@deriving repr]
end = struct
  type _ t = int [@@deriving repr]
end

let __ : type a. a typ -> a Phantom.t typ = Phantom.t

module Multiple : sig
  type ('a, 'b, 'c) t = { foo : 'a; bar : 'b list; baz : 'b * 'c }
  [@@deriving repr]
end = struct
  type ('a, 'b, 'c) t = { foo : 'a; bar : 'b list; baz : 'b * 'c }
  [@@deriving repr]
end

let __ : type a b c. a typ -> b typ -> c typ -> (a, b, c) Multiple.t typ =
  Multiple.t
