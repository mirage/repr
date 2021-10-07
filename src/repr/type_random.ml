open Type_core
open Staging
module R = Random.State

module Attr = Attribute.Make1 (struct
  type 'a t = R.t -> 'a

  let name = "random"
end)

type 'a random = (R.t -> 'a) staged

let ( let+ ) x f =
  let x = unstage x in
  stage (f x)

let ( and+ ) a b = stage (unstage a, unstage b)
let return x = stage (fun _ -> x)

(* Sample lengths according to a geometric distribution by inverse
   transform sampling an exponential one (characterised by [mean_length = λ⁻¹])
   and rounding to integer values. *)
let pick_len : mean:int -> len -> R.t -> int =
  let bound len x =
    match len with
    | `Int | `Int64 -> x
    | `Int8 -> min x ((1 lsl 8) - 1)
    | `Int16 -> min x ((1 lsl 16) - 1)
    | `Int32 -> min x ((0x7fff lsl 16) lor 0xffff)
  in
  fun ~mean l s ->
    match l with
    | (`Int | `Int8 | `Int16 | `Int32 | `Int64) as l ->
        bound l (Float.to_int (-.Float.log (R.float s 1.) *. Float.of_int mean))
    | `Fixed i -> i

let indexable :
    type a b.
    mean_len:int -> len -> (int -> (int -> a) -> b) -> a random -> b random =
 fun ~mean_len len init elt ->
  let+ elt = elt in
  fun s -> init (pick_len ~mean:mean_len len s) (fun _ -> elt s)

module Record_deriver = Fields_folder (struct
  type ('a, 'b) t = R.t -> 'b -> 'a
end)

let int32 =
  let open Int32 in
  let bits s = of_int (R.bits s) in
  fun s -> logxor (bits s) (shift_left (bits s) 30)

let int64 =
  let open Int64 in
  let bits s = of_int (R.bits s) in
  fun s ->
    logxor (bits s) (logxor (shift_left (bits s) 30) (shift_left (bits s) 60))

let int =
  match Sys.word_size with
  | 64 -> fun s -> Int64.to_int (int64 s)
  | 32 -> fun s -> Int32.to_int (int32 s)
  | _ -> assert false

let float s =
  R.float s (if R.bool s then Float.max_float else -.Float.max_float)

let rec t : type a. a t -> a random = function
  | Map x -> map x
  | Prim x -> prim x
  | Tuple x -> tuple x
  | List { len; v } -> indexable ~mean_len:4 len List.init (t v)
  | Array { len; v } -> indexable ~mean_len:4 len Array.init (t v)
  | Option x -> option x
  | Record x -> record x
  | Variant x -> variant x
  | Attributes { attrs; attr_type } -> (
      match Attr.find attrs with Some f -> stage f | None -> t attr_type)
  | Boxed x -> t x
  | Self x -> stage (fun s -> (* improperly staged *) unstage (t x.self_fix) s)
  | Custom _ -> failwith "Cannot generate random instance of Custom type"
  | Var v -> raise (Unbound_type_variable v)

and char : char random = stage (fun s -> Char.unsafe_chr (R.int s 256))

and prim : type a. a prim -> a random = function
  | Unit -> return ()
  | Bool -> stage R.bool
  | Char -> char
  | Int -> stage int
  | Int32 -> stage int32
  | Int64 -> stage int64
  | Float -> stage float
  | String len -> indexable ~mean_len:8 len String.init char
  | Bytes len -> indexable ~mean_len:8 len Bytes.init char

and tuple : type a. a tuple -> a random = function
  | Pair (a, b) ->
      let+ a = t a and+ b = t b in
      fun s -> (a s, b s)
  | Triple (a, b, c) ->
      let+ a = t a and+ b = t b and+ c = t c in
      fun s -> (a s, b s, c s)

and option : type a. a t -> a option random =
 fun elt ->
  let+ elt = t elt in
  fun s -> match R.bool s with true -> None | false -> Some (elt s)

and record : type a. a record -> a random =
 fun { rfields = Fields (fs, constr); _ } ->
  let nil _ v = v in
  let cons { ftype; _ } random_remaining =
    let f_random = unstage (t ftype) in
    fun s constr ->
      let f = f_random s in
      random_remaining s (constr f)
  in
  let f = Record_deriver.fold { nil; cons } fs in
  stage (fun s -> f s constr)

and variant : type a. a variant -> a random =
 fun v ->
  let random_case =
    let cases = Array.length v.vcases in
    fun s -> R.int s cases
  in
  let generators =
    ArrayLabels.map v.vcases ~f:(function
      | C0 { c0; _ } -> fun _ -> c0
      | C1 { c1; ctype1; _ } ->
          let inner = unstage (t ctype1) in
          fun s -> c1 (inner s))
  in
  stage (fun s -> generators.(random_case s) s)

and map : type a b. (a, b) map -> b random =
 fun m ->
  let+ inner = t m.x in
  fun s -> m.f (inner s)

let of_state = t

let of_global ty =
  let+ random = of_state ty in
  fun () -> random (Random.get_state ())
