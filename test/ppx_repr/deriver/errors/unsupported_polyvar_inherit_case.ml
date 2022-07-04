type 'a s = [ `S of int | `R ] [@@deriving repr]
type my_polyvar = [ int s | `T of string ] [@@deriving repr]
