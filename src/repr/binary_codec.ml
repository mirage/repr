include Binary_codec_intf
open Staging

let unsafe_add_bytes b k = k (Bytes.unsafe_to_string b)
let str = Bytes.unsafe_of_string

let charstring_of_code : int -> string =
  let tbl =
    Array.init 256 (fun i -> Bytes.unsafe_to_string (Bytes.make 1 (Char.chr i)))
  in
  fun [@inline always] i ->
    assert (i < 256);
    Array.unsafe_get tbl i

module Unit = struct
  let encode () _k = ()
  let decode _ ofs = (ofs, ()) [@@inline always]
end

module Char = struct
  let encode c k = k (charstring_of_code (Char.code c))
  let decode buf ofs = (ofs + 1, buf.[ofs]) [@@inline always]
end

module Bool = struct
  let encode b = Char.encode (if b then '\255' else '\000')

  let decode buf ofs =
    let ofs, c = Char.decode buf ofs in
    match c with '\000' -> (ofs, false) | _ -> (ofs, true)
end

module Int8 = struct
  let encode i k = k (charstring_of_code i)

  let decode buf ofs =
    let ofs, c = Char.decode buf ofs in
    (ofs, Stdlib.Char.code c)
    [@@inline always]
end

module Int16 = struct
  let encode i =
    let b = Bytes.create 2 in
    Bytes.set_uint16_be b 0 i;
    unsafe_add_bytes b

  let decode buf ofs = (ofs + 2, Bytes.get_uint16_be (str buf) ofs)
end

module Int32 = struct
  let encode i =
    let b = Bytes.create 4 in
    Bytes.set_int32_be b 0 i;
    unsafe_add_bytes b

  let decode buf ofs = (ofs + 4, Bytes.get_int32_be (str buf) ofs)
end

module Int64 = struct
  let encode i =
    let b = Bytes.create 8 in
    Bytes.set_int64_be b 0 i;
    unsafe_add_bytes b

  let decode buf ofs = (ofs + 8, Bytes.get_int64_be (str buf) ofs)
end

module Float = struct
  let encode f = Int64.encode (Stdlib.Int64.bits_of_float f)

  let decode buf ofs =
    let ofs, f = Int64.decode buf ofs in
    (ofs, Stdlib.Int64.float_of_bits f)
end

module Int = struct
  let encode i k =
    let rec aux n k =
      if n >= 0 && n < 128 then k (charstring_of_code n)
      else
        let out = 128 lor (n land 127) in
        k (charstring_of_code out);
        aux (n lsr 7) k
    in
    aux i k

  let decode buf ofs =
    let rec aux buf n p ofs =
      let ofs, i = Int8.decode buf ofs in
      let n = n + ((i land 127) lsl p) in
      if i >= 0 && i < 128 then (ofs, n) else aux buf n (p + 7) ofs
    in
    aux buf 0 0 ofs
end

module Len = struct
  let encode n i =
    match n with
    | `Int -> Int.encode i
    | `Int8 -> Int8.encode i
    | `Int16 -> Int16.encode i
    | `Int32 -> Int32.encode (Stdlib.Int32.of_int i)
    | `Int64 -> Int64.encode (Stdlib.Int64.of_int i)
    | `Fixed _ -> Unit.encode ()
    | `Unboxed -> Unit.encode ()

  let decode n buf ofs =
    match n with
    | `Int -> Int.decode buf ofs
    | `Int8 -> Int8.decode buf ofs
    | `Int16 -> Int16.decode buf ofs
    | `Int32 ->
        let ofs, i = Int32.decode buf ofs in
        (ofs, Stdlib.Int32.to_int i)
    | `Int64 ->
        let ofs, i = Int64.decode buf ofs in
        (ofs, Stdlib.Int64.to_int i)
    | `Fixed n -> (ofs, n)
    | `Unboxed -> (ofs, String.length buf - ofs)
end

(* Helper functions generalising over [string] / [bytes]. *)
module Mono_container = struct
  let decode_unboxed of_string of_bytes =
    stage @@ fun buf ofs ->
    let len = String.length buf - ofs in
    if ofs = 0 then (len, of_string buf)
    else
      let str = Bytes.create len in
      String.blit buf ofs str 0 len;
      (ofs + len, of_bytes str)

  let decode of_string of_bytes =
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
        let ofs, len = Len.decode n buf ofs in
        sub len buf ofs
end

module String_unboxed = struct
  let encode _ = stage (fun s k -> k s)

  let decode _ =
    Mono_container.decode_unboxed (fun x -> x) Bytes.unsafe_to_string
end

module Bytes_unboxed = struct
  (* NOTE: makes a copy of [b], since [k] may retain the string it's given *)
  let encode _ = stage (fun b k -> k (Bytes.to_string b))

  let decode _ =
    Mono_container.decode_unboxed Bytes.unsafe_of_string (fun x -> x)
end

module String = struct
  let encode len =
    stage (fun s k ->
        let i = String.length s in
        Len.encode len i k;
        k s)

  let decode len = Mono_container.decode (fun x -> x) Bytes.unsafe_to_string len
end

module Bytes = struct
  let encode len =
    stage (fun s k ->
        let i = Bytes.length s in
        Len.encode len i k;
        unsafe_add_bytes s k)

  let decode len = Mono_container.decode Bytes.unsafe_of_string (fun x -> x) len
end

module Option = struct
  let encode encode_elt v k =
    match v with
    | None -> Char.encode '\000' k
    | Some x ->
        Char.encode '\255' k;
        encode_elt x k

  let decode decode_elt buf ofs =
    let ofs, c = Char.decode buf ofs in
    match c with
    | '\000' -> (ofs, None)
    | _ ->
        let ofs, x = decode_elt buf ofs in
        (ofs, Some x)
end

module List = struct
  let encode =
    let rec encode_elements encode_elt k = function
      | [] -> ()
      | x :: xs ->
          encode_elt x k;
          (encode_elements [@tailcall]) encode_elt k xs
    in
    fun len encode_elt ->
      stage (fun x k ->
          Len.encode len (List.length x) k;
          encode_elements encode_elt k x)

  let decode =
    let rec decode_elements decode_elt acc buf off = function
      | 0 -> (off, List.rev acc)
      | n ->
          let off, x = decode_elt buf off in
          decode_elements decode_elt (x :: acc) buf off (n - 1)
    in
    fun len decode_elt ->
      stage (fun buf ofs ->
          let ofs, len = Len.decode len buf ofs in
          decode_elements decode_elt [] buf ofs len)
end

module Array = struct
  let encode =
    let encode_elements encode_elt k arr =
      for i = 0 to Array.length arr - 1 do
        encode_elt (Array.unsafe_get arr i) k
      done
    in
    fun n l ->
      stage (fun x k ->
          Len.encode n (Array.length x) k;
          encode_elements l k x)

  let decode len decode_elt =
    let list_decode = unstage (List.decode len decode_elt) in
    stage (fun buf off ->
        let ofs, l = list_decode buf off in
        (ofs, Array.of_list l))
end

module Pair = struct
  let encode a b (x, y) k =
    a x k;
    b y k

  let decode a b buf off =
    let off, a = a buf off in
    let off, b = b buf off in
    (off, (a, b))
end

module Triple = struct
  let encode a b c (x, y, z) k =
    a x k;
    b y k;
    c z k

  let decode a b c buf off =
    let off, a = a buf off in
    let off, b = b buf off in
    let off, c = c buf off in
    (off, (a, b, c))
end
