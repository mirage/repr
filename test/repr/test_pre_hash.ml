(* Test that an overridden [pre_hash] function nested inside a large type is
   used correctly. *)
let test_nested_custom () =
  let module X = struct
    (* A type that stores its [pre_hash] directly: *)
    type custom = { pre_hash : string; ignored_data : int } [@@deriving repr]

    let custom_t =
      let pre_hash = Repr.stage (fun { pre_hash; _ } f -> f pre_hash) in
      Repr.like ~pre_hash custom_t

    type pair = custom * custom [@@deriving repr]
  end in
  let pre_hash_string =
    let writer = Repr.(unstage (pre_hash X.pair_t)) in
    let buf = Buffer.create 0 in
    fun x ->
      writer x (Buffer.add_string buf);
      Buffer.contents buf
  in
  let input =
    ( { X.pre_hash = "a"; ignored_data = 0 },
      { X.pre_hash = "b"; ignored_data = 0 } )
  in
  let expected_output =
    (* Pre-hash of the pair is the concatenation of the precomputed component
       pre-hashes. *)
    "ab"
  in
  Alcotest.(check ~pos:__POS__ (testable Fmt.Dump.string String.equal))
    "" expected_output (pre_hash_string input)

let tests = [ ("nested custom", `Quick, test_nested_custom) ]
