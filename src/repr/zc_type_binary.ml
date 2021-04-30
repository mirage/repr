include Zc_type_core
open Staging
open Utils

module ZC_encode (OC : Output_channel) = struct
  include Zc_type_core.Types (OC)

  let byte oc n = OC.output_char oc (Char.unsafe_chr n)
  let unit _oc () = ()
  let add_string oc s = OC.output_string oc s
  let char oc c = byte oc (Char.code c)
  let unsafe_add_bytes oc b = OC.output_bytes oc b

  let int8 oc i =
    assert (i < 256);
    byte oc i

  let int16 oc i =
    OC.output_byte oc (i lsr 8);
    OC.output_byte oc i

  let int32 oc (i : int32) =
    let i = Int32.to_int i in
    for s = 3 downto 0 do
      OC.output_byte oc (i lsr (s * 8))
    done

  let int64 oc (i : int64) =
    let i = Int64.to_int i in
    for s = 7 downto 0 do
      OC.output_byte oc (i lsr (s * 8))
    done

  let float oc f = int64 oc (Int64.bits_of_float f)
  let bool oc b = char oc (if b then '\255' else '\000')

  let int oc i =
    let rec aux n =
      if n >= 0 && n < 128 then byte oc n
      else
        let out = 128 lor (n land 127) in
        byte oc out;
        aux (n lsr 7)
    in
    aux i

  let len oc n i =
    match n with
    | `Int -> int oc i
    | `Int8 -> int8 oc i
    | `Int16 -> int16 oc i
    | `Int32 -> int32 oc (Int32.of_int i)
    | `Int64 -> int64 oc (Int64.of_int i)
    | `Fixed _ -> unit oc ()
    | `Unboxed -> unit oc ()

  let unboxed_string _ = stage add_string

  let boxed_string n =
    stage @@ fun oc s ->
    let i = String.length s in
    len oc n i;
    add_string oc s

  let string boxed = if boxed then boxed_string else unboxed_string
  let unboxed_bytes _ = stage @@ fun oc b -> add_string oc (Bytes.to_string b)

  let boxed_bytes n =
    stage @@ fun oc s ->
    let i = Bytes.length s in
    len oc n i;
    unsafe_add_bytes oc s

  let bytes boxed = if boxed then boxed_bytes else unboxed_bytes

  let list l n =
    let l = unstage l in
    stage (fun oc x ->
        len oc n (List.length x);
        List.iter (fun e -> l oc e) x)

  let array l n =
    let l = unstage l in
    stage (fun oc x ->
        len oc n (Array.length x);
        Array.iter (fun e -> l oc e) x)

  let pair a b =
    let a = unstage a and b = unstage b in
    stage (fun oc (x, y) ->
        a oc x;
        b oc y)

  let triple a b c =
    let a = unstage a and b = unstage b and c = unstage c in
    stage (fun oc (x, y, z) ->
        a oc x;
        b oc y;
        c oc z)

  let option o =
    let o = unstage o in
    stage (fun oc v ->
        match v with
        | None -> char oc '\000'
        | Some x ->
            char oc '\255';
            o oc x)

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
    stage (fun oc u -> encode_bin oc (g u))

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
    let field_encoders : (OC.out_channel -> a -> unit) list =
      fields r
      |> List.map @@ fun (Field f) ->
         let field_encode = unstage (t f.ftype) in
         fun oc x -> field_encode oc (f.fget x)
    in
    stage (fun oc x -> List.iter (fun f -> f oc x) field_encoders)

  (* and variant : type a. a variant -> a encode_bin =
   *   let c0 oc { ctag0; _ } = stage (int oc ctag0) in
   *   let c1 c =
   *     let encode_arg = unstage (t c.ctype1) in
   *     stage (fun oc v ->
   *         int oc c.ctag1;
   *         encode_arg oc v)
   *   in
   *   fun v -> fold_variant { c0; c1 } v *)

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
