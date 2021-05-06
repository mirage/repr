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

open Type_core
open Staging
include Type_binary_intf

module Make (IO : IO_channel) = struct
  type out_channel = IO.out_channel
  type in_channel = IO.in_channel
  type 'a bin_seq = 'a -> out_channel -> unit
  type 'a encode_bin = 'a bin_seq staged

  (* type 'a pre_hash = 'a bin_seq staged *)
  type 'a decode_bin = (in_channel -> int -> int * 'a) staged

  module Encode = struct
    let byte n oc = IO.append_char oc (Char.unsafe_chr n)
    let unit () _oc = ()
    let add_string s oc = IO.append_string oc s
    let char c oc = byte (Char.code c) oc
    let unsafe_add_bytes b oc = IO.append_bytes oc b

    let int8 i oc =
      assert (i < 256);
      byte i oc

    let int16 i oc =
      IO.append_byte oc (i lsr 8);
      IO.append_byte oc i

    let int32 (i : int32) oc =
      let i = Int32.to_int i in
      for s = 3 downto 0 do
        IO.append_byte oc (i lsr (s * 8))
      done

    let int64 (i : int64) oc =
      let i = Int64.to_int i in
      for s = 7 downto 0 do
        IO.append_byte oc (i lsr (s * 8))
      done

    let float f = int64 (Int64.bits_of_float f)
    let bool b = char (if b then '\255' else '\000')

    let int i oc =
      let rec aux n =
        if n >= 0 && n < 128 then byte n oc
        else
          let out = 128 lor (n land 127) in
          byte out oc;
          aux (n lsr 7)
      in
      aux i

    let len n i oc =
      match n with
      | `Int -> int i oc
      | `Int8 -> int8 i oc
      | `Int16 -> int16 i oc
      | `Int32 -> int32 (Int32.of_int i) oc
      | `Int64 -> int64 (Int64.of_int i) oc
      | `Fixed _ -> unit () oc
      | `Unboxed -> unit () oc

    let unboxed_string _ = stage add_string

    let boxed_string n =
      stage @@ fun s oc ->
      let i = String.length s in
      len n i oc;
      add_string s oc

    let string boxed = if boxed then boxed_string else unboxed_string
    let unboxed_bytes _ = stage @@ fun b oc -> add_string (Bytes.to_string b) oc

    let boxed_bytes n =
      stage @@ fun s oc ->
      let i = Bytes.length s in
      len n i oc;
      unsafe_add_bytes s oc

    let bytes boxed = if boxed then boxed_bytes else unboxed_bytes

    let list l n =
      let l = unstage l in
      stage (fun x oc ->
          len n (List.length x) oc;
          List.iter (fun e -> l e oc) x)

    let array l n =
      let l = unstage l in
      stage (fun x oc ->
          len n (Array.length x) oc;
          Array.iter (fun e -> l e oc) x)

    let pair a b =
      let a = unstage a and b = unstage b in
      stage (fun (x, y) oc ->
          a x oc;
          b y oc)

    let triple a b c =
      let a = unstage a and b = unstage b and c = unstage c in
      stage (fun (x, y, z) oc ->
          a x oc;
          b y oc;
          c z oc)

    let option o =
      let o = unstage o in
      stage (fun v oc ->
          match v with
          | None -> char '\000' oc
          | Some x ->
              char '\255' oc;
              o x oc)

    let rec t : type a. a t -> a encode_bin = function
      | Self _s -> failwith "Not implemented for IO_channel" (* fst (self s) *)
      | Custom _ -> failwith "Not implemented for IO_channel"
      | Map b -> map ~boxed:true b
      | Prim t -> prim ~boxed:true t
      | Boxed b -> t b
      | List l -> list (t l.v) l.len
      | Array a -> array (t a.v) a.len
      | Tuple t -> tuple t
      | Option x -> option (t x)
      | Record r -> record r
      | Variant _v -> failwith "TODO" (* variant v *)
      | Var v -> raise (Unbound_type_variable v)

    and unboxed : type a. a t -> a encode_bin = function
      | Self _s -> failwith "Not implemented for IO_channel" (* snd (self s) *)
      | Custom _ -> failwith "Not implemented for IO_channel"
      | Map b -> map ~boxed:false b
      | Prim t -> prim ~boxed:false t
      | Boxed b -> t b
      | List l -> list (t l.v) l.len
      | Array a -> array (t a.v) a.len
      | Tuple t -> tuple t
      | Option x -> option (t x)
      | Record r -> record r
      | Variant _v -> failwith "TODO" (* variant v *)
      | Var v -> raise (Unbound_type_variable v)

    (* and self : type a. a self -> a encode_bin * a encode_bin =
     *  fun { self_unroll; _ } ->
     *   zc_fix_staged (fun encode_bin unboxed_encode_bin ->
     *       let cyclic =
     *         self_unroll (partial ~encode_bin ~unboxed_encode_bin ())
     *       in
     *       (t cyclic, unboxed cyclic)) *)
    and tuple : type a. a tuple -> a encode_bin = function
      | Pair (x, y) -> pair (t x) (t y)
      | Triple (x, y, z) -> triple (t x) (t y) (t z)

    and map : type a b. boxed:bool -> (a, b) map -> b encode_bin =
     fun ~boxed { x; g; _ } ->
      let encode_bin = unstage (if boxed then t x else unboxed x) in
      stage (fun u oc -> encode_bin (g u) oc)

    and prim : type a. boxed:bool -> a prim -> a encode_bin =
     fun ~boxed -> function
      | Unit -> stage unit
      | Bool -> stage bool
      | Char -> stage char
      | Int -> stage int
      | Int32 -> stage int32
      | Int64 -> stage int64
      | Float -> stage float
      | String n -> string boxed n
      | Bytes n -> bytes boxed n

    and record : type a. a record -> a encode_bin =
     fun r ->
      let field_encoders : (a -> IO.out_channel -> unit) list =
        fields r
        |> List.map @@ fun (Field f) ->
           let field_encode = unstage (t f.ftype) in
           fun x -> field_encode (f.fget x)
      in
      stage (fun x oc -> List.iter (fun f -> f x oc) field_encoders)

    (* and variant : type a. a variant -> a encode_bin =
     *   let c0 { ctag0; _ } = stage (int ctag0) in
     *   let c1 c =
     *     let encode_arg = unstage (t c.ctype1) in
     *     stage (fun v oc ->
     *         int c.ctag1 oc;
     *         encode_arg v oc)
     *   in
     *   fun v -> fold_variant { c0; c1 } v *)
  end

  module Decode = struct
    type 'a res = int * 'a

    let unit _ ofs = (ofs, ()) [@@inline always]
    let char ic ofs = (ofs + 1, IO.input_char ic ofs) [@@inline always]

    let int8 buf ofs =
      let ofs, c = char buf ofs in
      (ofs, Char.code c)
      [@@inline always]

    let int16 ic ofs =
      let v = (IO.input_byte ic ofs lsl 8) + IO.input_byte ic (ofs + 1) in
      (ofs + 2, v)

    let int32 ic ofs =
      let v =
        (IO.input_byte ic ofs lsl 24)
        + (IO.input_byte ic (ofs + 1) lsl 16)
        + (IO.input_byte ic (ofs + 2) lsl 8)
        + IO.input_byte ic (ofs + 3)
      in
      (ofs + 4, Int32.of_int v)

    let int64 ic ofs =
      let v =
        (IO.input_byte ic ofs lsl 56)
        + (IO.input_byte ic (ofs + 1) lsl 48)
        + (IO.input_byte ic (ofs + 2) lsl 40)
        + (IO.input_byte ic (ofs + 3) lsl 32)
        + (IO.input_byte ic (ofs + 4) lsl 24)
        + (IO.input_byte ic (ofs + 5) lsl 16)
        + (IO.input_byte ic (ofs + 6) lsl 8)
        + IO.input_byte ic (ofs + 7)
      in
      (ofs + 8, Int64.of_int v)

    let bool ic ofs =
      let ofs, c = char ic ofs in
      match c with '\000' -> (ofs, false) | _ -> (ofs, true)

    let float ic ofs =
      let ofs, f = int64 ic ofs in
      (ofs, Int64.float_of_bits f)

    let int ic ofs =
      let rec aux ic n p ofs =
        let ofs, i = int8 ic ofs in
        let n = n + ((i land 127) lsl p) in
        if i >= 0 && i < 128 then (ofs, n) else aux ic n (p + 7) ofs
      in
      aux ic 0 0 ofs

    let len ic ofs = function
      | `Int -> int ic ofs
      | `Int8 -> int8 ic ofs
      | `Int16 -> int16 ic ofs
      | `Int32 ->
          let ofs, i = int32 ic ofs in
          (ofs, Int32.to_int i)
      | `Int64 ->
          let ofs, i = int64 ic ofs in
          (ofs, Int64.to_int i)
      | `Fixed n -> (ofs, n)
      | `Unboxed -> failwith "TODO"
    (* (ofs, String.length ic - ofs) *)

    let mk_unboxed _of_bytes _ = failwith "TODO"
    (* stage @@ fun (ic : IO.in_channel) ofs ->
     * let len = String.length buf - ofs in
     * if ofs = 0 then (len, of_string buf)
     * else
     *   let str = Bytes.create len in
     *   String.blit buf ofs str 0 len;
     *   (ofs + len, of_bytes str) *)

    let mk_boxed of_bytes s =
      let sub len ic ofs =
        let str = Bytes.create len in
        IO.blit ic ofs str 0 len;
        (ofs + len, of_bytes str)
      in
      stage @@ fun ic ofs ->
      let ofs, len = len ic ofs s in
      sub len ic ofs

    let mk of_bytes =
      let f_boxed = mk_boxed of_bytes in
      let f_unboxed = mk_unboxed of_bytes in
      fun boxed -> if boxed then f_boxed else f_unboxed

    let string = mk Bytes.unsafe_to_string
    let bytes = mk (fun x -> x)

    let list l n =
      let l = unstage l in
      stage (fun (ic : IO.in_channel) ofs ->
          let ofs, len = len ic ofs n in
          let rec aux acc ofs = function
            | 0 -> (ofs, List.rev acc)
            | n ->
                let ofs, x = l ic ofs in
                aux (x :: acc) ofs (n - 1)
          in
          aux [] ofs len)

    let array l len =
      let decode_list = unstage (list l len) in
      stage (fun (ic : IO.in_channel) ofs ->
          let ofs, l = decode_list ic ofs in
          (ofs, Array.of_list l))

    let pair a b =
      let a = unstage a and b = unstage b in
      stage (fun (ic : IO.in_channel) ofs ->
          let ofs, a = a ic ofs in
          let ofs, b = b ic ofs in
          (ofs, (a, b)))

    let triple a b c =
      let a = unstage a and b = unstage b and c = unstage c in
      stage (fun (ic : IO.in_channel) ofs ->
          let ofs, a = a ic ofs in
          let ofs, b = b ic ofs in
          let ofs, c = c ic ofs in
          (ofs, (a, b, c)))

    let option : type a. a decode_bin -> a option decode_bin =
     fun o ->
      let o = unstage o in
      stage (fun (ic : IO.in_channel) ofs ->
          let ofs, c = char ic ofs in
          match c with
          | '\000' -> (ofs, None)
          | _ ->
              let ofs, x = o ic ofs in
              (ofs, Some x))

    module Record_decoder = Fields_folder (struct
      type ('a, 'b) t = IO.in_channel -> int -> 'b -> 'a res
    end)

    let rec t : type a. a t -> a decode_bin = function
      | Self _s -> failwith "Not implemented for IO_channel" (* fst (self s) *)
      | Custom _ -> failwith "Not implemented for IO_channel"
      | Map b -> map ~boxed:true b
      | Prim t -> prim ~boxed:true t
      | Boxed b -> t b
      | List l -> list (t l.v) l.len
      | Array a -> array (t a.v) a.len
      | Tuple t -> tuple t
      | Option x -> option (t x)
      | Record r -> record r
      | Variant v -> variant v
      | Var v -> raise (Unbound_type_variable v)

    and unboxed : type a. a t -> a decode_bin = function
      | Self _s -> failwith "Not implemented for IO_channel" (* snd (self s) *)
      | Custom _ -> failwith "Not implemented for IO_channel"
      | Map b -> map ~boxed:false b
      | Prim t -> prim ~boxed:false t
      | Boxed b -> t b
      | List l -> list (t l.v) l.len
      | Array a -> array (t a.v) a.len
      | Tuple t -> tuple t
      | Option x -> option (t x)
      | Record r -> record r
      | Variant v -> variant v
      | Var v -> raise (Unbound_type_variable v)

    (* and self : type a. a self -> a decode_bin * a decode_bin =
     *  fun { self_unroll; _ } ->
     *   fix_staged2 (fun decode_bin unboxed_decode_bin ->
     *       let cyclic =
     *         self_unroll (partial ~decode_bin ~unboxed_decode_bin ())
     *       in
     *       (t cyclic, unboxed cyclic)) *)
    and tuple : type a. a tuple -> a decode_bin = function
      | Pair (x, y) -> pair (t x) (t y)
      | Triple (x, y, z) -> triple (t x) (t y) (t z)

    and map : type a b. boxed:bool -> (a, b) map -> b decode_bin =
     fun ~boxed { x; f; _ } ->
      let decode_bin = unstage (if boxed then t x else unboxed x) in
      stage (fun (ic : IO.in_channel) ofs ->
          let ofs, x = decode_bin ic ofs in
          (ofs, f x))

    and prim : type a. boxed:bool -> a prim -> a decode_bin =
     fun ~boxed -> function
      | Unit -> stage unit
      | Bool -> stage bool
      | Char -> stage char
      | Int -> stage int
      | Int32 -> stage int32
      | Int64 -> stage int64
      | Float -> stage float
      | String n -> string boxed n
      | Bytes n -> bytes boxed n

    and record : type a. a record -> a decode_bin =
     fun { rfields = Fields (fs, constr); _ } ->
      let nil _ic ofs f = (ofs, f) in
      let cons { ftype; _ } decode_remaining =
        let f_decode = unstage (t ftype) in
        fun (ic : IO.in_channel) ofs constr ->
          let ofs, x = f_decode ic ofs in
          let constr = constr x in
          decode_remaining ic ofs constr
      in
      let f = Record_decoder.fold { nil; cons } fs in
      stage (fun (ic : IO.in_channel) ofs -> f ic ofs constr)

    and variant : type a. a variant -> a decode_bin =
     fun v ->
      let decoders : a decode_bin array =
        v.vcases
        |> Array.map @@ function
           | C0 c -> stage (fun _ ofs -> (ofs, c.c0))
           | C1 c ->
               let decode_arg = unstage (t c.ctype1) in
               stage (fun (ic : IO.in_channel) ofs ->
                   let ofs, x = decode_arg ic ofs in
                   (ofs, c.c1 x))
      in
      stage (fun (ic : IO.in_channel) ofs ->
          let ofs, i = int ic ofs in
          unstage decoders.(i) ic ofs)
  end

  let encode_bin = Encode.t
  let decode_bin = Decode.t

  module Unboxed = struct
    let encode_bin = Encode.unboxed
    let decode_bin = Decode.unboxed
  end
end

type 'a to_bin_string = 'a to_string staged
type 'a of_bin_string = 'a of_string staged

module Buffer = struct
  type out_channel = Buffer.t
  type in_channel = Buffer.t

  include Buffer

  let append_char = Buffer.add_char
  let append_string = Buffer.add_string
  let append_bytes = Buffer.add_bytes
  let append_byte buf byt = Buffer.add_char buf (Char.chr byt)

  (** [input_byte ic off] is [byte] *)
  let input_byte buf pos = Char.code @@ Buffer.nth buf pos

  (** [input_char ic off] is [char] *)
  let input_char = Buffer.nth

  let blit = Buffer.blit
end

module Serde_buffer = Make (Buffer)

let to_bin size_of encode_bin =
  let size_of = unstage size_of in
  let encode_bin = unstage encode_bin in
  stage (fun x ->
      let seq = encode_bin x in
      let len = match size_of x with None -> 1024 | Some n -> n in
      let buf = Buffer.create len in
      seq buf;
      Buffer.contents buf)

let map_result f = function Ok x -> Ok (f x) | Error _ as e -> e

let to_bin_string =
  let rec aux : type a. a t -> a to_bin_string =
   fun t ->
    match t with
    | Self s -> aux s.self_fix
    | Map m ->
        let mapped = unstage (aux m.x) in
        stage (fun x -> mapped (m.g x))
    | Prim (String _) -> stage (fun x -> x)
    | Prim (Bytes _) -> stage Bytes.to_string
    | Custom _c -> failwith "Not implemented for IO_channel"
    | _ -> to_bin (Type_size.unboxed t) (Serde_buffer.Encode.unboxed t)
  in
  aux

let of_bin decode_bin x =
  let buf = Buffer.create 0 in
  Buffer.add_string buf x;
  let last, v = decode_bin buf 0 in
  assert (last = String.length x);
  Ok v

let of_bin_string t =
  let rec aux : type a. a t -> a of_bin_string =
   fun t ->
    match t with
    | Self s -> aux s.self_fix
    | Map l ->
        let mapped = unstage (aux l.x) in
        stage (fun x -> mapped x |> map_result l.f)
    | Prim (String _) -> stage (fun x -> Ok x)
    | Prim (Bytes _) -> stage (fun x -> Ok (Bytes.of_string x))
    | Custom _ -> failwith "Not implemented for IO_channel"
    | _ -> stage (of_bin (unstage (Serde_buffer.Decode.unboxed t)))
  in
  let f = unstage (aux t) in
  stage (fun x -> try f x with Invalid_argument e -> Error (`Msg e))
