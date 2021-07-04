open Ppxlib

let ( >> ) f g x = g (f x)
let ( >|= ) x f = List.map f x

module Option = struct
  include Option

  let to_bool : unit option -> bool = function Some () -> true | None -> false
end

module List = struct
  include List

  (* TODO(4.10): remove *)
  let concat_map =
    let rec aux f acc = function
      | [] -> rev acc
      | x :: l ->
          let xs = f x in
          aux f (List.rev_append xs acc) l
    in
    fun ~f l -> aux f [] l

  let reduce ~f = function
    | [] -> None
    | [ x ] -> Some x
    | _ :: _ :: _ as l ->
        let rec aux = function
          | [] -> assert false
          | [ a; b ] -> f a b
          | x :: xs -> f x (aux xs)
        in
        Some (aux l)

  let reduce_exn ~f l =
    match reduce ~f l with
    | Some x -> x
    | None -> failwith "Cannot reduce empty list"
end

module Make (A : Ast_builder.S) : sig
  val compose_all : ('a -> 'a) list -> 'a -> 'a
  (** Left-to-right composition of a list of functions. *)

  val lambda : string list -> expression -> expression
  (** [lambda \[ "x_1"; ...; "x_n" \] e] is [fun x1 ... x_n -> e] *)

  val arrow : core_type list -> core_type -> core_type
  (** [arrow \[ "t_1"; ...; "t_n" \] u] is [t_1 -> ... -> t_n -> u] *)
end = struct
  open A

  let compose_all l x = List.fold_left ( |> ) x (List.rev l)
  let lambda = List.map (pvar >> pexp_fun Nolabel None) >> compose_all
  let arrow = List.map (ptyp_arrow Nolabel) >> compose_all
end
