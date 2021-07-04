module T0 : sig
  type nonrec t = int

  and other = string
  [@@deriving
    repr ~equal ~compare ~pp ~pp_dump ~size_of ~to_bin_string ~of_bin_string
      ~encode_bin ~decode_bin]
end = struct
  type nonrec t = int

  and other = string
  [@@deriving
    repr ~equal ~compare ~pp ~pp_dump ~size_of ~to_bin_string ~of_bin_string
      ~encode_bin ~decode_bin]
end

module T1 : sig
  type 'a t = 'a list [@@deriving repr ~equal]
end = struct
  type 'a t = 'a list [@@deriving repr ~equal]
end

module T2 : sig
  type ('a, 'b) t = ('a * 'b) list [@@deriving repr ~equal]
end = struct
  type ('a, 'b) t = ('a * 'b) list [@@deriving repr ~equal]
end
