(*
 * Copyright (c) 2019-2020 Craig Ferguson <me@craigfe.io>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Ppxlib
open! Utils
include Engine_intf

let map_lident f = function
  | Lapply _ -> invalid_arg "Lident.Lapply not supported"
  | Ldot (l, s) -> Ldot (l, f s)
  | Lident s -> Lident (f s)

let repr_name_of_type_name = function "t" -> "t" | x -> x ^ "_t"

module Located (Attributes : Attributes.S) (A : Ast_builder.S) : S = struct
  type state = {
    rec_flag : rec_flag;
    type_name : string;
    lib : string option;
    repr_name : string;
    rec_detected : bool ref;
    var_repr : ([ `Any | `Var of string ] -> expression option) ref;
        (** Given a type variable in a type, get its corresponding typerep (if
            the variable is properly bound). *)
  }

  let add_var_repr : type a b. (a -> b option) ref -> a * b -> unit =
   fun f_ref (a, b) ->
    let f_old = !f_ref in
    let f_new a' = if a = a' then Some b else f_old a' in
    f_ref := f_new

  open Utils
  open Utils.Make (A)
  module Reader = Monad.Reader

  module Algebraic = struct
    include Algebraic
    include Algebraic.Located (A) (Reader)
  end

  open A
  open Reader.Syntax
  open Reader

  let all_unlabelled = List.map (fun x -> (Nolabel, x))

  let recursive ~lib fparam e =
    let mu = evar (match lib with Some s -> s ^ ".mu" | None -> "mu") in
    [%expr [%e mu] (fun [%p pvar fparam] -> [%e e])]

  type name = { typ : string; repr : string }

  let mutually_recursive ~lib (e1, n1) (e2, n2) =
    let mu2 = evar (match lib with Some s -> s ^ ".mu2" | None -> "mu2") in
    [%expr
      [%e mu2] (fun [%p pvar n1.repr] [%p pvar n2.repr] -> ([%e e1], [%e e2]))]

  let in_lib ~lib x = match lib with Some lib -> lib ^ "." ^ x | None -> x

  let contains_tvar tvar typ =
    (object
       inherit [bool] Ast_traverse.fold as super

       method! core_type_desc t =
         super#core_type_desc t >> fun acc ->
         acc || match t with Ptyp_var v when v = tvar -> true | _ -> false
    end)
      #core_type
      typ false

  let rowfield_is_inherit = function
    | { prf_desc = Rinherit _; _ } -> true
    | _ -> false

  let rec derive_core typ =
    let* { type_name; lib; var_repr; _ } = ask in
    let loc = typ.ptyp_loc in
    match typ.ptyp_desc with
    | Ptyp_constr ({ txt = const_name; _ }, args) -> (
        match Attribute.get Attributes.repr typ with
        | Some e -> return e
        | None ->
            let nobuiltin =
              Option.to_bool (Attribute.get Attributes.nobuiltin typ)
            in
            let* lident = derive_lident ~nobuiltin const_name in
            let+ cons_args =
              args >|= derive_core |> sequence |> map all_unlabelled
            in
            pexp_apply (pexp_ident lident) cons_args)
    | Ptyp_variant (_, Open, _) -> Raise.Unsupported.type_open_polyvar ~loc typ
    | Ptyp_variant (rowfields, Closed, _labellist) ->
        if List.exists rowfield_is_inherit rowfields then
          Raise.Unsupported.polyvar_inherit_case ~loc typ;
        derive_polyvariant type_name rowfields
    | Ptyp_poly _ -> Raise.Unsupported.type_poly ~loc typ
    | Ptyp_tuple args -> derive_tuple args
    | Ptyp_arrow _ -> Raise.Unsupported.type_arrow ~loc typ
    | Ptyp_any -> Location.raise_errorf ~loc "Unbound type variable"
    | Ptyp_var v -> (
        match !var_repr (`Var v) with
        | Some r -> return r
        | None -> Location.raise_errorf ~loc "Unbound type variable" v)
    | Ptyp_package _ -> Raise.Unsupported.type_package ~loc typ
    | Ptyp_extension _ -> Raise.Unsupported.type_extension ~loc typ
    | Ptyp_alias (c, var) ->
        if contains_tvar var c then (
          add_var_repr var_repr (`Var var, evar var);
          let+ inner = derive_core c in
          recursive ~lib var inner)
        else derive_core c
    | Ptyp_object _ | Ptyp_class _ -> invalid_arg "unsupported"

  and derive_tuple args =
    let* { lib; _ } = ask in
    match args with
    | [ t ] ->
        (* This case can occur when the tuple type is nested inside a variant *)
        derive_core t
    | _ ->
        let tuple_type =
          (match List.length args with
          | 2 -> "pair"
          | 3 -> "triple"
          | n -> Raise.Unsupported.tuple_size ~loc n)
          |> in_lib ~lib
          |> evar
        in
        args
        >|= derive_core
        |> sequence
        |> map (all_unlabelled >> pexp_apply tuple_type)

  and derive_record ls =
    let* { type_name; lib; _ } = ask in
    let subderive label_decl =
      let field_name = label_decl.pld_name.txt in
      let+ field_repr = derive_core label_decl.pld_type in
      Algebraic.Typ.{ field_name; field_repr }
    in
    Algebraic.(encode Typ.Record) ~subderive ~lib ~type_name ls

  and derive_variant cs =
    let* { type_name; lib; _ } = ask in
    let subderive c =
      let case_name = c.pcd_name.txt in
      let+ case_cons =
        match c.pcd_args with
        | Pcstr_record _ -> invalid_arg "Inline record types unsupported"
        | Pcstr_tuple [] -> return None
        | Pcstr_tuple cs ->
            let+ tuple_typ = derive_tuple cs in
            Some (tuple_typ, List.length cs)
      in
      Algebraic.Typ.{ case_name; case_cons }
    in
    Algebraic.(encode Variant) ~subderive ~lib ~type_name cs

  and derive_polyvariant name rowfields =
    let* { lib; _ } = ask in
    let subderive f =
      let+ case_name, case_cons =
        match f.prf_desc with
        | Rtag (label, _, []) -> return (label.txt, None)
        | Rtag (label, _, typs) ->
            let+ tuple_typ = derive_tuple typs in
            (label.txt, Some (tuple_typ, List.length typs))
        | Rinherit _ -> assert false
      in
      Algebraic.Typ.{ case_name; case_cons }
    in
    Algebraic.(encode Polyvariant) ~subderive ~lib ~type_name:name rowfields

  and derive_lident :
      nobuiltin:bool -> longident -> (longident loc, state) Reader.t =
   fun ~nobuiltin txt ->
    let+ { lib; type_name; rec_flag; rec_detected; repr_name; _ } = ask in
    match (rec_flag, txt) with
    | Recursive, Lident const_name when String.equal const_name type_name ->
        (* If this type is the one we are deriving and the 'nonrec'
           keyword hasn't been used, replace with the repr
           name *)
        rec_detected := true;
        Located.lident repr_name
    | _ -> (
        match (nobuiltin, Dsl.type_to_combinator_name txt) with
        | true, (Some _ | None) | false, None ->
            map_lident repr_name_of_type_name txt |> Located.mk
        | false, Some combinator_name ->
            in_lib ~lib combinator_name |> Located.lident)

  let derive_type_decl : type_declaration -> (expression, state) Reader.t =
   fun typ ->
    match typ.ptype_kind with
    | Ptype_abstract -> (
        match typ.ptype_manifest with
        | None -> invalid_arg "No manifest"
        | Some c -> (
            match c.ptyp_desc with
            (* No need to open library module *)
            | Ptyp_constr ({ txt; loc = _ }, []) -> (
                match Attribute.get Attributes.repr c with
                | Some repr -> return repr
                | None ->
                    let nobuiltin =
                      match Attribute.get Attributes.nobuiltin c with
                      | Some () -> true
                      | None -> false
                    in
                    let+ name = derive_lident ~nobuiltin txt in
                    pexp_ident name)
            (* Type constructor: list, tuple, etc. *)
            | _ -> derive_core c))
    | Ptype_variant cs -> derive_variant cs
    | Ptype_record ls -> derive_record ls
    | Ptype_open -> Raise.Unsupported.type_open ~loc

  let parse_lib expr =
    let pattern =
      let open Ast_pattern in
      let none = map0 ~f:None @@ pexp_construct (lident (string "None")) none in
      let some =
        map1 ~f:Option.some
        @@ pexp_construct (lident (string "Some")) (some (estring __))
      in
      none ||| some
    in
    Ast_pattern.parse pattern loc expr
      (fun k -> k)
      ~on_error:(fun () ->
        Location.raise_errorf ~loc:expr.pexp_loc
          "Could not process `lib' argument: must be either `Some \"Lib\"' or \
           `None'")

  (* Remove duplicate elements from a list (preserving the order of the first
     occurrence of each duplicate). *)
  let list_uniq_stable =
    let rec inner ~seen acc = function
      | [] -> List.rev acc
      | x :: xs when not (List.mem x seen) ->
          inner ~seen:(x :: seen) (x :: acc) xs
      | _ :: xs (* seen *) -> inner ~seen acc xs
    in
    inner ~seen:[] []

  module Unbound_tvars = struct
    type acc = { free : string list; ctx_bound : string list }

    (* Find all unbound type variables, renaming any instances of [Ptyp_any] to a
       fresh variable. *)
    let find typ =
      (object
         inherit [acc] Ast_traverse.fold_map as super

         method! core_type_desc t acc =
           match t with
           | Ptyp_var v when not (List.mem v acc.ctx_bound) ->
               (t, { acc with free = v :: acc.free })
           | Ptyp_any ->
               let name = gen_symbol () in
               (Ptyp_var name, { acc with free = name :: acc.free })
           | Ptyp_alias (c, v) ->
               (* Push [v] to the bound stack, traverse the alias, then remove it. *)
               let c, acc =
                 super#core_type c { acc with ctx_bound = v :: acc.ctx_bound }
               in
               let ctx_bound =
                 match acc.ctx_bound with
                 | v' :: ctx_bound when v = v' -> ctx_bound
                 | _ -> assert false
               in
               (Ptyp_alias (c, v), { acc with ctx_bound })
           | _ -> super#core_type_desc t acc
      end)
        #core_type
        typ
        { free = []; ctx_bound = [] }
  end

  let expand_typ ?lib typ =
    let typ, Unbound_tvars.{ free = tvars; _ } = Unbound_tvars.find typ in
    let tvars = List.rev tvars |> list_uniq_stable in
    let env =
      {
        rec_flag = Nonrecursive;
        type_name = "t";
        repr_name = "t";
        rec_detected = ref false;
        lib;
        var_repr =
          ref (function
            | `Any ->
                assert false
                (* We already renamed all instances of [Ptyp_any] *)
            | `Var x -> Some (evar x));
      }
    in
    run (derive_core typ) env |> lambda tvars

  let derive_sig ~plugins ~name ~lib (_rec_flag, type_declarations) =
    List.concat_map type_declarations ~f:(fun typ ->
        let type_name = typ.ptype_name.txt in
        let name =
          Located.mk
            (match name with
            | Some n -> n
            | None -> repr_name_of_type_name type_name)
        in
        let ty_lident =
          (match lib with
          | Some _ -> in_lib ~lib "t"
          | None -> (
              (* This type decl may shadow the repr type ['a t] *)
              match name.txt with "t" -> "ty" | _ -> "t"))
          |> Located.lident
        in
        let type_ =
          combinator_type_of_type_declaration typ ~f:(fun ~loc:_ t ->
              ptyp_constr ty_lident [ t ])
        in
        let plugin_derivations =
          let td = name_type_params_in_td typ in
          let params =
            ListLabels.map td.ptype_params ~f:(function v, _ ->
                ptyp_constr ty_lident [ v ])
          in
          let ctyp = core_type_of_type_declaration td in
          ListLabels.map plugins
            ~f:(Meta_deriving.Plugin.derive_sig ~loc ~type_name ~params ~ctyp)
        in
        psig_value (value_description ~name ~type_ ~prim:[])
        :: plugin_derivations)

  module Typerep_derivation = struct
    type t = { params : string list; body : expression }
    (** A typerep derivation is an expression defined in terms of combinators,
        potentially scoped inside a list of parameters (if the corresponding
        type has type parameters). *)

    let to_expr ?(transform_body = Fun.id) t =
      lambda t.params (transform_body t.body)
  end

  let repr_of_type_decl ~(handle_recursion : bool) ~(lib : string option)
      ~rec_flag typ repr_name :
      pattern * Typerep_derivation.t * [ `Param_required of bool ] =
    let tparams =
      typ.ptype_params
      |> List.map (function
           | { ptyp_desc = Ptyp_var v; _ }, _ -> v
           | { ptyp_desc = Ptyp_any; _ }, _ -> "_"
           | _ -> assert false)
    in
    let env =
      let type_name = typ.ptype_name.txt in
      let rec_detected = ref false in
      let var_repr =
        ref (function
          | `Any -> Raise.Unsupported.type_any ~loc
          | `Var v -> if List.mem v tparams then Some (evar v) else None)
      in
      { rec_flag; type_name; repr_name; rec_detected; lib; var_repr }
    in
    let expr = run (derive_type_decl typ) env in
    (* If the type is syntactically self-referential, wrap with [mu] *)
    let expr =
      if handle_recursion && !(env.rec_detected) then
        recursive ~lib:env.lib env.repr_name expr
      else expr
    in
    let expr = Typerep_derivation.{ params = tparams; body = expr } in
    let pat = pvar env.repr_name in
    (pat, expr, `Param_required (List.length tparams > 0))

  let derive_str ~plugins ~name ~lib = function
    | Recursive, [] -> assert false
    | Recursive, tds when List.length tds > 2 ->
        failwith "Mutually-recursive groups of size > 2 supported"
    | rec_flag, type_declarations ->
        let multiple_tds = List.length type_declarations > 1 in
        let repr_names =
          match (name, type_declarations) with
          | _, [] -> assert false
          | Some _, _ :: _ :: _ ->
              failwith "Cannot specify name of mutually-recursive group"
          | Some repr, [ typ ] -> [ { repr; typ = typ.ptype_name.txt } ]
          | None, _ ->
              ListLabels.map type_declarations ~f:(fun typ ->
                  let typ = typ.ptype_name.txt in
                  let repr = repr_name_of_type_name typ in
                  { typ; repr })
        in

        let pats, named_treps =
          (* If there is only one type declaration – and it's potentially
             recursive – we might want to add a [mu] combinator inside the repr
             derivation.

             Mutually-recursive declarations are handled separately with [mu2]
             combinators below. *)
          let handle_recursion = rec_flag = Recursive && not multiple_tds in
          ListLabels.map2 type_declarations repr_names ~f:(fun typ name ->
              let pat, expr, `Param_required pr =
                repr_of_type_decl ~rec_flag ~handle_recursion ~lib typ name.repr
              in
              if pr && multiple_tds then
                failwith
                  "Can't support mutually-recursive types with type parameters";
              (pat, (expr, name)))
          |> List.split
        in
        let pat, expr =
          match (pats, named_treps) with
          | [ p1 ], [ (e1, _) ] -> (p1, Typerep_derivation.to_expr e1)
          | ps, es ->
              let pat =
                List.reduce_exn ps ~f:(fun p1 p2 -> [%pat? [%p p1], [%p p2]])
              in
              let expr =
                if rec_flag = Recursive then
                  match (es : (Typerep_derivation.t * name) list) with
                  | [ (e1, n1); (e2, n2) ] ->
                      (* Mutual recursion with type parameters rejected above *)
                      assert (e1.params = []);
                      assert (e2.params = []);
                      mutually_recursive ~lib (e1.body, n1) (e2.body, n2)
                  | _ ->
                      (* Recursive groups of size n > 2 rejected above *)
                      assert false
                else
                  List.map (fst >> Typerep_derivation.to_expr) es
                  |> List.reduce_exn ~f:(fun e1 e2 -> [%expr [%e e1], [%e e2]])
              in
              (pat, expr)
        in
        let plugin_derivations =
          List.concat_map named_treps ~f:(fun (typerep, name) ->
              ListLabels.map plugins ~f:(fun plugin ->
                  Meta_deriving.Plugin.derive_str ~loc ~type_name:name.typ
                    ~params:typerep.Typerep_derivation.params
                    ~expr:typerep.Typerep_derivation.body plugin))
        in
        pstr_value Nonrecursive [ value_binding ~pat ~expr ]
        :: plugin_derivations
end
