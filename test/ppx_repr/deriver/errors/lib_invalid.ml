type t = unit [@@deriving repr { lib = "foo" }] (* should be [Some "foo"] *)
