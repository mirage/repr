type s = [ `S ] [@@deriving repr]
type t = [ s | `T ] [@@deriving repr]
