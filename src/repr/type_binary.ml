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
open Utils

module Encode = struct
  let chars =
    Array.init 256 (fun i -> Bytes.unsafe_to_string (Bytes.make 1 (Char.chr i)))

  let unit () _k = ()
  let unsafe_add_bytes b k = k (Bytes.unsafe_to_string b)
  let add_string s k = k s
  let char c k = k chars.(Char.code c)

  let int8 i k =
    assert (i < 256);
    k chars.(i)

  let int16 i =
    let b = Bytes.create 2 in
    Bytes.set_uint16_be b 0 i;
    unsafe_add_bytes b

  let int32 i =
    let b = Bytes.create 4 in
    Bytes.set_int32_be b 0 i;
    unsafe_add_bytes b

  let int64 i =
    let b = Bytes.create 8 in
    Bytes.set_int64_be b 0 i;
    unsafe_add_bytes b

  let float f = int64 (Int64.bits_of_float f)
  let bool b = char (if b then '\255' else '\000')

  let int i k =
    let rec aux n k =
      if n >= 0 && n < 128 then k chars.(n)
      else
        let out = 128 lor (n land 127) in
        k chars.(out);
        aux (n lsr 7) k
    in
    aux i k

  let len n i =
    match n with
    | `Int -> int i
    | `Int8 -> int8 i
    | `Int16 -> int16 i
    | `Int32 -> int32 (Int32.of_int i)
    | `Int64 -> int64 (Int64.of_int i)
    | `Fixed _ -> unit ()
    | `Unboxed -> unit ()

  let unboxed_string _ = stage add_string

  let boxed_string n =
    let len = len n in
    stage @@ fun s k ->
    let i = String.length s in
    len i k;
    add_string s k

  let string boxed = if boxed then boxed_string else unboxed_string
  let unboxed_bytes _ = stage @@ fun b k -> add_string (Bytes.to_string b) k

  let boxed_bytes n =
    let len = len n in
    stage @@ fun s k ->
    let i = Bytes.length s in
    len i k;
    unsafe_add_bytes s k

  let bytes boxed = if boxed then boxed_bytes else unboxed_bytes

  let list l n =
    let l = unstage l in
    stage (fun x k ->
        len n (List.length x) k;
        List.iter (fun e -> l e k) x)

  let array l n =
    let l = unstage l in
    stage (fun x k ->
        len n (Array.length x) k;
        Array.iter (fun e -> l e k) x)

  let pair a b =
    let a = unstage a and b = unstage b in
    stage (fun (x, y) k ->
        a x k;
        b y k)

  let triple a b c =
    let a = unstage a and b = unstage b and c = unstage c in
    stage (fun (x, y, z) k ->
        a x k;
        b y k;
        c z k)

  let option o =
    let o = unstage o in
    stage (fun v k ->
        match v with
        | None -> char '\000' k
        | Some x ->
            char '\255' k;
            o x k)

  let rec t : type a. a t -> a encode_bin = function
    | Self s -> fst (self s)
    | Custom c -> c.encode_bin
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

  and unboxed : type a. a t -> a encode_bin = function
    | Self s -> snd (self s)
    | Custom c -> c.unboxed_encode_bin
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

  and self : type a. a self -> a encode_bin * a encode_bin =
   fun { self_unroll; _ } ->
    fix_staged2 (fun encode_bin unboxed_encode_bin ->
        let cyclic = self_unroll (partial ~encode_bin ~unboxed_encode_bin ()) in
        (t cyclic, unboxed cyclic))

  and tuple : type a. a tuple -> a encode_bin = function
    | Pair (x, y) -> pair (t x) (t y)
    | Triple (x, y, z) -> triple (t x) (t y) (t z)

  and map : type a b. boxed:bool -> (a, b) map -> b encode_bin =
   fun ~boxed { x; g; _ } ->
    let encode_bin = unstage (if boxed then t x else unboxed x) in
    stage (fun u k -> encode_bin (g u) k)

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
    let field_encoders : (a -> (string -> unit) -> unit) list =
      fields r
      |> List.map @@ fun (Field f) ->
         let field_encode = unstage (t f.ftype) in
         fun x -> field_encode (f.fget x)
    in
    stage (fun x k -> List.iter (fun f -> f x k) field_encoders)

  and variant : type a. a variant -> a encode_bin =
    let c0 { ctag0; _ } = stage (int ctag0) in
    let c1 c =
      let encode_arg = unstage (t c.ctype1) in
      stage (fun v k ->
          int c.ctag1 k;
          encode_arg v k)
    in
    fun v -> fold_variant { c0; c1 } v
end

module Decode = struct
  type 'a res = int * 'a

  let unit _ ofs = (ofs, ()) [@@inline always]
  let char buf ofs = (ofs + 1, buf.[ofs]) [@@inline always]

  let int8 buf ofs =
    let ofs, c = char buf ofs in
    (ofs, Char.code c)
    [@@inline always]

  let str = Bytes.unsafe_of_string
  let int16 buf ofs = (ofs + 2, Bytes.get_uint16_be (str buf) ofs)
  let int32 buf ofs = (ofs + 4, Bytes.get_int32_be (str buf) ofs)
  let int64 buf ofs = (ofs + 8, Bytes.get_int64_be (str buf) ofs)

  let bool buf ofs =
    let ofs, c = char buf ofs in
    match c with '\000' -> (ofs, false) | _ -> (ofs, true)

  let float buf ofs =
    let ofs, f = int64 buf ofs in
    (ofs, Int64.float_of_bits f)

  let int buf ofs =
    let rec aux buf n p ofs =
      let ofs, i = int8 buf ofs in
      let n = n + ((i land 127) lsl p) in
      if i >= 0 && i < 128 then (ofs, n) else aux buf n (p + 7) ofs
    in
    aux buf 0 0 ofs

  let len buf ofs = function
    | `Int -> int buf ofs
    | `Int8 -> int8 buf ofs
    | `Int16 -> int16 buf ofs
    | `Int32 ->
        let ofs, i = int32 buf ofs in
        (ofs, Int32.to_int i)
    | `Int64 ->
        let ofs, i = int64 buf ofs in
        (ofs, Int64.to_int i)
    | `Fixed n -> (ofs, n)
    | `Unboxed -> (ofs, String.length buf - ofs)

  let mk_unboxed of_string of_bytes _ =
    stage @@ fun buf ofs ->
    let len = String.length buf - ofs in
    if ofs = 0 then (len, of_string buf)
    else
      let str = Bytes.create len in
      String.blit buf ofs str 0 len;
      (ofs + len, of_bytes str)

  let mk_boxed of_string of_bytes =
    let sub len buf ofs =
      if ofs = 0 && len = String.length buf then (len, of_string buf)
      else
        let str = Bytes.create len in
        String.blit buf ofs str 0 len;
        (ofs + len, of_bytes str)
    in
    function
    | `Fixed n ->
        (* fixed-size strings are never boxed *)
        stage @@ fun buf ofs -> sub n buf ofs
    | n ->
        stage @@ fun buf ofs ->
        let ofs, len = len buf ofs n in
        sub len buf ofs

  let mk of_string of_bytes =
    let f_boxed = mk_boxed of_string of_bytes in
    let f_unboxed = mk_unboxed of_string of_bytes in
    fun boxed -> if boxed then f_boxed else f_unboxed

  let string = mk (fun x -> x) Bytes.unsafe_to_string
  let bytes = mk Bytes.of_string (fun x -> x)

  let list l n =
    let l = unstage l in
    stage (fun buf ofs ->
        let ofs, len = len buf ofs n in
        let rec aux acc ofs = function
          | 0 -> (ofs, List.rev acc)
          | n ->
              let ofs, x = l buf ofs in
              aux (x :: acc) ofs (n - 1)
        in
        aux [] ofs len)

  let array l len =
    let decode_list = unstage (list l len) in
    stage (fun buf ofs ->
        let ofs, l = decode_list buf ofs in
        (ofs, Array.of_list l))

  let pair a b =
    let a = unstage a and b = unstage b in
    stage (fun buf ofs ->
        let ofs, a = a buf ofs in
        let ofs, b = b buf ofs in
        (ofs, (a, b)))

  let triple a b c =
    let a = unstage a and b = unstage b and c = unstage c in
    stage (fun buf ofs ->
        let ofs, a = a buf ofs in
        let ofs, b = b buf ofs in
        let ofs, c = c buf ofs in
        (ofs, (a, b, c)))

  let option : type a. a decode_bin -> a option decode_bin =
   fun o ->
    let o = unstage o in
    stage (fun buf ofs ->
        let ofs, c = char buf ofs in
        match c with
        | '\000' -> (ofs, None)
        | _ ->
            let ofs, x = o buf ofs in
            (ofs, Some x))

  module Record_decoder = Fields_folder (struct
    type ('a, 'b) t = string -> int -> 'b -> 'a res [@@deriving branded]
  end)

  let rec t : type a. a t -> a decode_bin = function
    | Self s -> fst (self s)
    | Custom c -> c.decode_bin
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
    | Self s -> snd (self s)
    | Custom c -> c.unboxed_decode_bin
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

  and self : type a. a self -> a decode_bin * a decode_bin =
   fun { self_unroll; _ } ->
    fix_staged2 (fun decode_bin unboxed_decode_bin ->
        let cyclic = self_unroll (partial ~decode_bin ~unboxed_decode_bin ()) in
        (t cyclic, unboxed cyclic))

  and tuple : type a. a tuple -> a decode_bin = function
    | Pair (x, y) -> pair (t x) (t y)
    | Triple (x, y, z) -> triple (t x) (t y) (t z)

  and map : type a b. boxed:bool -> (a, b) map -> b decode_bin =
   fun ~boxed { x; f; _ } ->
    let decode_bin = unstage (if boxed then t x else unboxed x) in
    stage (fun buf ofs ->
        let ofs, x = decode_bin buf ofs in
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
    let nil _buf ofs f = (ofs, f) in
    let cons { ftype; _ } decode_remaining =
      let f_decode = unstage (t ftype) in
      fun buf ofs constr ->
        let ofs, x = f_decode buf ofs in
        let constr = constr x in
        decode_remaining buf ofs constr
    in
    let f = Record_decoder.fold { nil; cons } fs in
    stage (fun buf ofs -> f buf ofs constr)

  and variant : type a. a variant -> a decode_bin =
   fun v ->
    let decoders : a decode_bin array =
      v.vcases
      |> Array.map @@ function
         | C0 c -> stage (fun _ ofs -> (ofs, c.c0))
         | C1 c ->
             let decode_arg = unstage (t c.ctype1) in
             stage (fun buf ofs ->
                 let ofs, x = decode_arg buf ofs in
                 (ofs, c.c1 x))
    in
    stage (fun buf ofs ->
        let ofs, i = int buf ofs in
        unstage decoders.(i) buf ofs)
end

let encode_bin = Encode.t
let decode_bin = Decode.t

type 'a to_bin_string = 'a to_string staged
type 'a of_bin_string = 'a of_string staged

module Unboxed = struct
  let encode_bin = Encode.unboxed
  let decode_bin = Decode.unboxed
end

let to_bin size_of encode_bin =
  let size_of = unstage size_of in
  let encode_bin = unstage encode_bin in
  stage (fun x ->
      let seq = encode_bin x in
      let len = match size_of x with None -> 1024 | Some n -> n in
      let buf = Buffer.create len in
      seq (Buffer.add_string buf);
      Buffer.contents buf)

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
    | Custom c -> to_bin c.unboxed_size_of c.unboxed_encode_bin
    | _ -> to_bin (Type_size.unboxed t) (Encode.unboxed t)
  in
  aux

let map_result f = function Ok x -> Ok (f x) | Error _ as e -> e

let of_bin decode_bin x =
  let last, v = decode_bin x 0 in
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
    | Custom c -> stage (of_bin (unstage c.unboxed_decode_bin))
    | _ -> stage (of_bin (unstage (Decode.unboxed t)))
  in
  let f = unstage (aux t) in
  stage (fun x -> try f x with Invalid_argument e -> Error (`Msg e))
