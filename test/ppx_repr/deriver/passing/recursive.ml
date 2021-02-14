(* Non-recursive group of type declarations: *)

module Non_recursive_group : sig
  type nonrec t0

  and t1

  and t2 [@@deriving repr]
end = struct
  type nonrec t0 = unit

  and t1 = unit

  and t2 = unit [@@deriving repr]
end

module Recursive_group : sig
  type even

  and odd [@@deriving repr]
end = struct
  type odd = Odd of even option

  and even = Even of odd option [@@deriving repr]
end
