type t = unit [@@deriving repr]
type t_alias = unit [@@deriving repr]

(* Ensure that 'nonrec' assertions are respected *)
module S1 : sig
  type nonrec t = t list [@@deriving repr]
  type nonrec t_alias = t_alias list [@@deriving repr]
end = struct
  type nonrec t = t list [@@deriving repr]
  type nonrec t_alias = t_alias list [@@deriving repr]
end

(* Now test the interaction of 'nonrec' with custom naming *)
module S2 : sig
  type nonrec t = t list [@@deriving repr { name = "t_repr" }]
  type nonrec t_alias = t_alias list [@@deriving repr { name = "t_repr" }]
end = struct
  type nonrec t = t list [@@deriving repr { name = "t_repr" }]
  type nonrec t_alias = t_alias list [@@deriving repr { name = "t_repr" }]
end
