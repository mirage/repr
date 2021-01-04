(*
 * Copyright (c) 2016-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

include Type_intf
include Type_core
include Type_combinators
include Staging

let pre_hash t =
  let rec aux : type a. a t -> a encode_bin = function
    | Self s -> aux s.self_fix
    | Map m ->
        let dst = unstage (aux m.x) in
        stage (fun v -> dst (m.g v))
    | Custom c -> c.pre_hash
    | t -> Type_binary.Unboxed.encode_bin t
  in
  aux t

let short_hash = function
  | Custom c -> c.short_hash
  | t ->
      let pre_hash = unstage (pre_hash t) in
      stage @@ fun ?seed x ->
      let seed = match seed with None -> 0 | Some t -> t in
      let h = ref seed in
      pre_hash x (fun s -> h := Hashtbl.seeded_hash !h s);
      !h

let v ~pp ~of_string ~json ~bin ?unboxed_bin ?bin_codec_uuid ~equal ~compare
    ~short_hash ~pre_hash () =
  let encode_json, decode_json = json in
  let encode_bin, decode_bin, size_of = bin in
  let unboxed_encode_bin, unboxed_decode_bin, unboxed_size_of =
    match unboxed_bin with None -> bin | Some b -> b
  in
  Custom
    {
      cwit = `Witness (Witness.make ());
      pp;
      of_string;
      pre_hash;
      encode_json;
      decode_json;
      encode_bin;
      decode_bin;
      size_of;
      compare;
      equal;
      short_hash;
      unboxed_encode_bin;
      unboxed_decode_bin;
      unboxed_size_of;
      bin_codec_uuid;
    }

let like ?pp ?of_string ?json ?bin ?unboxed_bin ?bin_codec_uuid ?equal ?compare
    ?short_hash:h ?pre_hash:p t =
  let or_default ~op:generic_op = function
    | Some x -> x
    | None -> generic_op t
  in
  let encode_json, decode_json =
    let ( >|= ) l f = match l with Ok l -> Ok (f l) | Error _ as e -> e in
    let join = function Error _ as e -> e | Ok x -> x in
    match json with
    | Some (x, y) -> (x, y)
    | None -> (
        let rec is_prim : type a. a t -> bool = function
          | Self s -> is_prim s.self_fix
          | Map m -> is_prim m.x
          | Prim _ -> true
          | _ -> false
        in
        match (t, pp, of_string) with
        | ty, Some pp, Some of_string when is_prim ty ->
            let ty = string in
            ( (fun ppf u -> Type_json.encode ty ppf (Fmt.to_to_string pp u)),
              fun buf -> Type_json.decode ty buf >|= of_string |> join )
        | _ -> (Type_json.encode t, Type_json.decode t))
  in
  let pp = or_default ~op:Type_pp.t pp in
  let of_string = or_default ~op:Type_pp.of_string of_string in
  let encode_bin, decode_bin, size_of =
    match bin with
    | Some (x, y, z) -> (x, y, z)
    | None -> (Type_binary.encode_bin t, Type_binary.decode_bin t, Type_size.t t)
  in
  let unboxed_encode_bin, unboxed_decode_bin, unboxed_size_of =
    match unboxed_bin with
    | Some (x, y, z) -> (x, y, z)
    | None ->
        ( Type_binary.Unboxed.encode_bin t,
          Type_binary.Unboxed.decode_bin t,
          Type_size.unboxed t )
  in
  let equal =
    match equal with
    | Some x -> x
    | None -> (
        match compare with
        | Some f ->
            let f = unstage f in
            stage (fun x y -> f x y = 0)
        | None -> Type_ordered.equal t)
  in
  let compare = or_default ~op:Type_ordered.compare compare in
  let short_hash = match h with Some x -> x | None -> short_hash t in
  let pre_hash = match p with Some x -> x | None -> encode_bin in
  Custom
    {
      cwit = `Type t;
      pp;
      of_string;
      encode_json;
      decode_json;
      encode_bin;
      decode_bin;
      size_of;
      compare;
      equal;
      short_hash;
      pre_hash;
      unboxed_encode_bin;
      unboxed_decode_bin;
      unboxed_size_of;
      bin_codec_uuid;
    }

let map ?pp ?of_string ?json ?bin ?unboxed_bin ?bin_codec_uuid ?equal ?compare
    ?short_hash ?pre_hash ?uuid x f g =
  match
    ( pp,
      of_string,
      json,
      bin,
      unboxed_bin,
      bin_codec_uuid,
      equal,
      compare,
      short_hash,
      pre_hash )
  with
  | None, None, None, None, None, None, None, None, None, None ->
      Map { x; f; g; uuid; mwit = Witness.make () }
  | _ ->
      let x = Map { x; f; g; uuid; mwit = Witness.make () } in
      like ?pp ?of_string ?json ?bin ?unboxed_bin ?bin_codec_uuid ?equal
        ?compare ?short_hash ?pre_hash x

module type S = sig
  type t

  val t : t ty
end

let equal, compare = Type_ordered.(equal, compare)

let pp, pp_dump, pp_ty, to_string, of_string =
  Type_pp.(t, dump, ty, to_string, of_string)

let ( to_json_string,
      of_json_string,
      pp_json,
      encode_json,
      decode_json,
      decode_json_lexemes ) =
  Type_json.(to_string, of_string, pp, encode, decode_jsonm, decode_lexemes)

let encode_bin, decode_bin, to_bin_string, of_bin_string =
  Type_binary.(encode_bin, decode_bin, to_bin_string, of_bin_string)

let size_of = Type_size.t

module Unboxed = struct
  include Type_binary.Unboxed

  let size_of = Type_size.unboxed
end

module Binary_shape = Type_binary.Shape
module Uuid = Type_binary.Uuid
