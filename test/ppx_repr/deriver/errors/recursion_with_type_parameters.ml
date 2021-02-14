type 'a t1 = 'a * t2 option

and t2 = int * unit t1 option [@@deriving repr]
