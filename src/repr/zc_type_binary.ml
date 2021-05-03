include Zc_type_core
open Staging
open Utils

module ZC_encode (OC : Output_channel) = struct
  include Zc_type_core.Types (OC)

  let byte n oc = OC.output_char oc (Char.unsafe_chr n)
  let unit () _oc = ()
  let add_string s oc = OC.output_string oc s
  let char c oc = byte (Char.code c) oc
  let unsafe_add_bytes b oc = OC.output_bytes oc b

  let int8 i oc =
    assert (i < 256);
    byte i oc

  let int16 i oc =
    OC.output_byte oc (i lsr 8);
    OC.output_byte oc i

  let int32 (i : int32) oc =
    let i = Int32.to_int i in
    for s = 3 downto 0 do
      OC.output_byte oc (i lsr (s * 8))
    done

  let int64 (i : int64) oc =
    let i = Int64.to_int i in
    for s = 7 downto 0 do
      OC.output_byte oc (i lsr (s * 8))
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
    | Variant _v -> failwith "TODO" (* variant v *)
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
    | Variant _v -> failwith "TODO" (* variant v *)
    | Var v -> raise (Unbound_type_variable v)

  and self : type a. a self -> a encode_bin * a encode_bin =
   fun { self_unroll; _ } ->
    fix_staged_oc2 (fun encode_bin unboxed_encode_bin ->
        let cyclic = self_unroll (partial ~encode_bin ~unboxed_encode_bin ()) in
        (t cyclic, unboxed cyclic))

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
    let field_encoders : (a -> OC.out_channel -> unit) list =
      fields r
      |> List.map @@ fun (Field f) ->
         let field_encode = unstage (t f.ftype) in
         fun x -> field_encode (f.fget x)
    in
    stage (fun x oc -> List.iter (fun f -> f x oc) field_encoders)

  and variant : type a. a variant -> a encode_bin =
    let c0 { ctag0; _ } = stage (int ctag0) in
    let c1 c =
      let encode_arg = unstage (t c.ctype1) in
      stage (fun v oc ->
          int c.ctag1 oc;
          encode_arg v oc)
    in
    fun v -> fold_variant { c0; c1 } v

  let encode_bin = t
  let decode_bin = t
  let unit = Prim Unit
  let bool = Prim Bool
  let char = Prim Char
  let int = Prim Int
  let int32 = Prim Int32
  let int64 = Prim Int64
  let float = Prim Float
  let string = Prim (String `Int)
  let bytes = Prim (Bytes `Int)
end
