open Ppxlib

module Plugin = struct
  type t = {
    name : string;
    type_name : [ `before | `after ];
    impl : location -> expression -> expression;
    intf : location -> core_type -> core_type;
  }

  let create ?(type_name = `after) ~impl ~intf name =
    { name; type_name; impl; intf }

  let op_name_of_type_name t n =
    match (n, t.type_name) with
    | "t", _ -> t.name
    | x, `before -> Printf.sprintf "%s_%s" x t.name
    | x, `after -> Printf.sprintf "%s_%s" t.name x

  let derive_str t ~loc ~type_name ~params ~expr:repr =
    let (module Ast_builder) = Ast_builder.make loc in
    let open Ast_builder in
    let name = op_name_of_type_name t type_name in
    let expr =
      let body = t.impl loc repr in
      ListLabels.fold_right params ~init:body ~f:(fun p acc ->
          pexp_fun Nolabel None (pvar p) acc)
    in
    pstr_value Nonrecursive
      [ value_binding ~pat:(ppat_var (Located.mk name)) ~expr ]

  let derive_sig t ~loc ~type_name ~params ~ctyp:repr =
    let (module Ast_builder) = Ast_builder.make loc in
    let open Ast_builder in
    let name = op_name_of_type_name t type_name in
    let type_ =
      let return_type = t.intf loc repr in
      ListLabels.fold_right params ~init:return_type ~f:(ptyp_arrow Nolabel)
    in
    psig_value (value_description ~name:(Located.mk name) ~type_ ~prim:[])

  let defaults =
    [
      create "equal"
        ~impl:(fun loc t -> [%expr Repr.unstage (Repr.equal [%e t])])
        ~intf:(fun loc t -> [%type: [%t t] -> [%t t] -> bool]);
      create "compare"
        ~impl:(fun loc t -> [%expr Repr.unstage (Repr.compare [%e t])])
        ~intf:(fun loc t -> [%type: [%t t] -> [%t t] -> int]);
      create "size_of"
        ~impl:(fun loc t -> [%expr Repr.unstage (Repr.size_of [%e t])])
        ~intf:(fun loc t -> [%type: [%t t] -> int option]);
      create "pp"
        ~impl:(fun loc t -> [%expr Repr.pp [%e t]])
        ~intf:(fun loc t -> [%type: Stdlib.Format.formatter -> [%t t] -> unit]);
      create "pp_dump"
        ~impl:(fun loc t -> [%expr Repr.pp_dump [%e t]])
        ~intf:(fun loc t -> [%type: Stdlib.Format.formatter -> [%t t] -> unit]);
      create "random"
        ~impl:(fun loc t -> [%expr Repr.unstage (Repr.random [%e t])])
        ~intf:(fun loc t -> [%type: unit -> [%t t]]);
      create "to_bin_string" ~type_name:`before
        ~impl:(fun loc t -> [%expr Repr.unstage (Repr.to_bin_string [%e t])])
        ~intf:(fun loc t -> [%type: [%t t] -> string]);
      create "of_bin_string" ~type_name:`before
        ~impl:(fun loc t -> [%expr Repr.unstage (Repr.of_bin_string [%e t])])
        ~intf:(fun loc t ->
          [%type: string -> ([%t t], [ `Msg of string ]) Stdlib.result]);
      create "encode_bin"
        ~impl:(fun loc t -> [%expr Repr.unstage (Repr.encode_bin [%e t])])
        ~intf:(fun loc t -> [%type: [%t t] -> (string -> unit) -> unit]);
      create "decode_bin"
        ~impl:(fun loc t -> [%expr Repr.unstage (Repr.decode_bin [%e t])])
        ~intf:(fun loc t -> [%type: string -> int ref -> [%t t]]);
      create "short_hash"
        ~impl:(fun loc t -> [%expr Repr.unstage (Repr.short_hash [%e t])])
        ~intf:(fun loc t -> [%type: ?seed:int -> [%t t] -> unit]);
      create "pre_hash"
        ~impl:(fun loc t -> [%expr Repr.unstage (Repr.pre_hash [%e t])])
        ~intf:(fun loc t -> [%type: [%t t] -> (string -> unit) -> unit]);
    ]
end

(** [Deriving.Args.t] is a heterogeneous list that supports only [revcons] but
    we need [cons] below. As a workaround, we use our own argument list type for
    the intermediate representation. *)
module Args = struct
  module Plain = Deriving.Args

  type (_, _) t =
    | [] : ('a, 'a) t
    | ( :: ) : 'a Plain.param * ('b, 'c) t -> ('a -> 'b, 'c) t

  let to_plain : type a b. (a, b) t -> (a, b) Plain.t =
    let rec aux : type a b c. (a, b) Plain.t -> (b, c) t -> (a, c) Plain.t =
     fun acc -> function [] -> acc | x :: xs -> aux Plain.(acc +> x) xs
    in
    fun t -> aux Deriving.Args.empty t

  let rec append : type a b c. (a, b) t -> (b, c) t -> (a, c) t =
   fun a b -> match a with [] -> b | x :: xs -> x :: append xs b
end

(** Each plugin gets a flag in the main deriver corresponding to whether it's
    activated or not. For instance, [\[@@deriving repr ~equal\]] indicates that
    the "equal" plugin should be run on this type definition.

    Given the list of plugins [ p1; p2; ... pn ], we need to build:

    - the [Deriving.Args] list of flags to pass to [Ppxlib];
    - a corresponding function over booleans [fun b1 b2 ... bn -> ...] for
      Ppxlib to call indicating which of the plugins have been activated.

    For each derivation, we pass the list of activated plugins to the deriver. *)
module Arg_collector = struct
  type _ t =
    | E : {
        args : ('f, 'output) Args.t;
        consumer : (Plugin.t list -> 'output) -> 'f;
      }
        -> 'output t

  let empty = E { args = Args.[]; consumer = (fun k -> k []) }

  let add (plugin : Plugin.t) (E { args; consumer }) =
    let args = Args.(Deriving.Args.flag plugin.name :: args) in
    let consumer k flag_passed =
      (* If this plugin has been selected, then add it to the list and pass it
         along, otherwise skip. *)
      consumer (fun ps -> if flag_passed then k (plugin :: ps) else k ps)
    in
    E { args; consumer }

  let for_plugins ps = ListLabels.fold_right ps ~f:add ~init:empty
end

let make_generator ?attributes ?deps ~args:extra_args ~supported_plugins f =
  let (E { args; consumer }) = Arg_collector.for_plugins supported_plugins in
  Deriving.Generator.make ?attributes ?deps
    Args.(to_plain (append args extra_args))
    (fun ~loc ~path input ->
      consumer (fun plugins -> f ~loc ~path plugins input))
