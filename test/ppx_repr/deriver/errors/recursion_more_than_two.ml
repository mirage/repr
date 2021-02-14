type t1 = t2 option

and t2 = t3 option

and t3 = t1 option [@@deriving repr]
