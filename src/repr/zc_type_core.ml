open Staging

module Dispatch = struct
  type 'a t =
    | Base : 'a staged -> 'a t
    | Arrow : { arg_wit : 'b Witness.t; f : ('b -> 'a) staged } -> 'a t
end

module type IO_channel = sig
  type out_channel

  val append_char : out_channel -> char -> unit
  val append_string : out_channel -> string -> unit
  val append_bytes : out_channel -> bytes -> unit
  val append_byte : out_channel -> int -> unit

  type in_channel

  val input_byte : in_channel -> int -> int
  (** [input_byte ic off] is [byte] *)

  val input_char : in_channel -> int -> char
  (** [input_char ic off] is [char] *)

  val blit : in_channel -> int -> bytes -> int -> int -> unit
end

module Types (IO : IO_channel) = struct
  type len = [ `Int | `Int8 | `Int16 | `Int32 | `Int64 | `Fixed of int ]
  type 'a pp = 'a Fmt.t
  type 'a of_string = string -> ('a, [ `Msg of string ]) result
  type 'a to_string = 'a -> string
  type 'a encode_json = Jsonm.encoder -> 'a -> unit
  type json_decoder = { mutable lexemes : Jsonm.lexeme list; d : Jsonm.decoder }
  type 'a decode_json = json_decoder -> ('a, [ `Msg of string ]) result
  type 'a bin_seq = 'a -> IO.out_channel -> unit
  type 'a pre_hash = 'a bin_seq staged
  type 'a encode_bin = 'a bin_seq staged
  type 'a decode_bin = (IO.in_channel -> int -> int * 'a) staged
  type 'a size_of = ('a -> int option) staged
  type 'a compare = ('a -> 'a -> int) staged
  type 'a equal = ('a -> 'a -> bool) staged
  type 'a short_hash = (?seed:int -> 'a -> int) staged

  type 'a t =
    | Var : string -> 'a t
    | Self : 'a self -> 'a t
    | Custom : 'a custom -> 'a t
    | Map : ('a, 'b) map -> 'b t
    | Prim : 'a prim -> 'a t
    | List : 'a len_v -> 'a list t
    | Array : 'a len_v -> 'a array t
    | Tuple : 'a tuple -> 'a t
    | Option : 'a t -> 'a option t
    | Record : 'a record -> 'a t
    | Variant : 'a variant -> 'a t
    | Boxed : 'a t -> 'a t

  and 'a len_v = { len : len; v : 'a t }

  and 'a custom = {
    cwit : [ `Type of 'a t | `Witness of 'a Witness.t ];
    pp : 'a pp;
    of_string : 'a of_string;
    encode_json : 'a encode_json;
    decode_json : 'a decode_json;
    short_hash : 'a short_hash;
    pre_hash : 'a encode_bin;
    compare : 'a compare;
    equal : 'a equal;
    (* boxed binary encoding *)
    encode_bin : 'a encode_bin;
    decode_bin : 'a decode_bin;
    size_of : 'a size_of;
    (* unboxed binary encoding *)
    unboxed_encode_bin : 'a encode_bin;
    unboxed_decode_bin : 'a decode_bin;
    unboxed_size_of : 'a size_of;
  }

  and ('a, 'b) map = {
    x : 'a t;
    f : 'a -> 'b;
    g : 'b -> 'a;
    mwit : 'b Witness.t;
  }

  and 'a self = { self_unroll : 'a t -> 'a t; mutable self_fix : 'a t }

  and 'a prim =
    | Unit : unit prim
    | Bool : bool prim
    | Char : char prim
    | Int : int prim
    | Int32 : int32 prim
    | Int64 : int64 prim
    | Float : float prim
    | String : len -> string prim
    | Bytes : len -> bytes prim

  and 'a tuple =
    | Pair : 'a t * 'b t -> ('a * 'b) tuple
    | Triple : 'a t * 'b t * 'c t -> ('a * 'b * 'c) tuple

  and 'a record = {
    rwit : 'a Witness.t;
    rname : string;
    rfields : 'a fields_and_constr;
  }

  and 'a fields_and_constr =
    | Fields : ('a, 'b) fields * 'b -> 'a fields_and_constr

  and ('a, 'b) fields =
    | F0 : ('a, 'a) fields
    | F1 : ('a, 'b) field * ('a, 'c) fields -> ('a, 'b -> 'c) fields

  and ('a, 'b) field = { fname : string; ftype : 'b t; fget : 'a -> 'b }

  and 'a variant = {
    vwit : 'a Witness.t;
    vname : string;
    vcases : 'a a_case array;
    vget : 'a -> 'a case_v;
  }

  and 'a a_case =
    | C0 : 'a case0 -> 'a a_case
    | C1 : ('a, 'b) case1 -> 'a a_case

  and 'a case_v =
    | CV0 : 'a case0 -> 'a case_v
    | CV1 : ('a, 'b) case1 * 'b -> 'a case_v

  and 'a case0 = { ctag0 : int; cname0 : string; c0 : 'a }

  and ('a, 'b) case1 = {
    ctag1 : int;
    cname1 : string;
    ctype1 : 'b t;
    cwit1 : 'b Witness.t;
    c1 : 'b -> 'a;
  }

  type 'a ty = 'a t

  exception Unbound_type_variable of string

  type _ a_field = Field : ('a, 'b) field -> 'a a_field

  module Case_folder = struct
    type ('a, 'f) t = {
      c0 : 'a case0 -> 'f staged;
      c1 : 'b. ('a, 'b) case1 -> ('b -> 'f) staged;
    }
  end

  let partial ?(pp = fun _ -> failwith "`pp` not implemented")
      ?(of_string = fun _ -> failwith "`of_string` not implemented")
      ?(encode_json = fun _ -> failwith "`encode_json` not implemented")
      ?(decode_json = fun _ -> failwith "`decode_json` not implemented")
      ?(short_hash =
        stage (fun ?seed:_ _ -> failwith "`short_hash` not implemented"))
      ?(pre_hash = stage (fun _ -> failwith "`pre_hash` not implemented"))
      ?(compare = stage (fun _ -> failwith "`compare` not implemented"))
      ?(equal = stage (fun _ -> failwith "`equal` not implemented"))
      ?(encode_bin = stage (fun _ -> failwith "`encode_bin` not implemented"))
      ?(decode_bin = stage (fun _ -> failwith "`decode_bin` not implemented"))
      ?(size_of = stage (fun _ -> failwith "`size_of` not implemented"))
      ?(unboxed_encode_bin =
        stage (fun _ -> failwith "`unboxed_encode_bin` not implemented"))
      ?(unboxed_decode_bin =
        stage (fun _ -> failwith "`unboxed_decode_bin` not implemented"))
      ?(unboxed_size_of =
        stage (fun _ -> failwith "`unboxed_size_of` not implemented")) () =
    Custom
      {
        cwit = `Witness (Witness.make ());
        pp;
        of_string;
        encode_json;
        decode_json;
        short_hash;
        pre_hash;
        compare;
        equal;
        encode_bin;
        decode_bin;
        size_of;
        unboxed_encode_bin;
        unboxed_decode_bin;
        unboxed_size_of;
      }

  let rec fields_aux : type a b. (a, b) fields -> a a_field list = function
    | F0 -> []
    | F1 (h, t) -> Field h :: fields_aux t

  let fields r = match r.rfields with Fields (f, _) -> fields_aux f

  let fold_variant :
      type a f. (a, f) Case_folder.t -> a variant -> (a -> f) staged =
   fun folder v_typ ->
    let cases =
      Array.map
        (function
          | C0 c0 -> Dispatch.Base (folder.c0 c0)
          | C1 c1 -> Dispatch.Arrow { arg_wit = c1.cwit1; f = folder.c1 c1 })
        v_typ.vcases
    in
    stage (fun v ->
        match v_typ.vget v with
        | CV0 { ctag0; _ } -> (
            match cases.(ctag0) with
            | Dispatch.Base x -> unstage x
            | _ -> assert false)
        | CV1 ({ ctag1; cwit1; _ }, v) -> (
            match cases.(ctag1) with
            | Dispatch.Arrow { f; arg_wit } -> (
                match Witness.cast cwit1 arg_wit v with
                | Some v -> unstage f v
                | None -> assert false)
            | _ -> assert false))

  module Fields_folder (Acc : sig
    type ('a, 'b) t
  end) =
  struct
    type 'a t = {
      nil : ('a, 'a) Acc.t;
      cons : 'b 'c. ('a, 'b) field -> ('a, 'c) Acc.t -> ('a, 'b -> 'c) Acc.t;
    }

    let rec fold : type a c. a t -> (a, c) fields -> (a, c) Acc.t =
     fun folder -> function
      | F0 -> folder.nil
      | F1 (f, fs) -> folder.cons f (fold folder fs)
  end
end