let check_string_eq pos ~expected actual =
  Alcotest.(check ~pos string) "" expected actual

let check_string_neq pos x y = Alcotest.(check ~pos (neg string)) "" x y

let to_to_string : type a. (a -> (string -> unit) -> unit) -> a -> string =
 fun encoder ->
  let buf = Buffer.create 0 in
  fun x ->
    let append_string = Buffer.add_string buf in
    encoder x append_string;
    let result = Buffer.contents buf in
    Buffer.clear buf;
    result

(* Test that an overridden [pre_hash] function nested inside a large type is
   used correctly. *)
let test_nested_custom () =
  let module X = struct
    (* A type that stores its [pre_hash] directly: *)
    type custom = { pre_hash : string; ignored_data : int } [@@deriving repr]

    let custom_t =
      let pre_hash { pre_hash; _ } f = f pre_hash in
      Repr.like ~pre_hash custom_t

    type pair = custom * custom [@@deriving repr ~pre_hash]
  end in
  let pre_hash_pair = to_to_string X.pre_hash_pair in
  let input =
    ( { X.pre_hash = "a"; ignored_data = 0 },
      { X.pre_hash = "b"; ignored_data = 0 } )
  in
  (* Pre-hash of the pair is the concatenation of the precomputed component
     pre-hashes. *)
  check_string_eq __POS__ ~expected:"ab" (pre_hash_pair input)

(* Tests that the pre-hashing function for a given representable type [t] is
   injective (i.e. that two non-equal values of type [t] always have non-equal
   pre-hashes). A non-injective pre-hash function would be subject to preimage
   attacks. *)
let test_injective () =
  let module X = struct
    type string_pair = string * string [@@deriving repr ~pre_hash]
  end in
  (* Test that pair components are boxed: *)
  let () =
    let pre_hash = to_to_string X.pre_hash_string_pair in
    let x = pre_hash ("a", "b") in
    let y = pre_hash ("ab", "") in
    check_string_neq __POS__ x y
  in
  ()

let tests =
  [
    ("nested custom", `Quick, test_nested_custom);
    ("injective", `Quick, test_injective);
  ]
