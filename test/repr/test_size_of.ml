module T = Repr

let encode_bin t = T.(unstage (encode_bin t))

let encode_bin typ v =
  let buffer = Buffer.create 0 in
  encode_bin typ v (Buffer.add_string buffer);
  Buffer.contents buffer

let random_string len = String.init len (fun _ -> char_of_int (Random.int 256))

let check_unknown ~__POS__:pos typ =
  match T.Size.of_value typ with
  | Unknown -> ()
  | Dynamic _ | Static _ ->
      Alcotest.failf ~pos
        "Expected type to have unknown size, but (Dynamic _ | Static _) was \
         received."

let check_static ~__POS__:pos typ expected v =
  match T.Size.of_value typ with
  | Unknown | Dynamic _ ->
      Alcotest.failf ~pos
        "Expected type to have static size %d, but (Unknown | Dynamic _) was \
         received."
        expected
  | Static n -> (
      Alcotest.(check ~pos int) "Expected static size" expected n;

      (* Check that the encoding actually occupies [n] bytes *)
      let actual_size = String.length (encode_bin typ v) in
      Alcotest.(check ~pos int)
        "Actual size must match static spec" expected actual_size;

      (* We require [∀ n. (of_value = Static n) ⇔ (of_encoding = Static n)] *)
      match T.Size.of_encoding typ with
      | Unknown | Dynamic _ ->
          Alcotest.failf ~pos
            "Type has a static [of_value] sizer, but a non-static \
             [of_encoding] sizer."
      | Static n' ->
          Alcotest.(check ~pos int) "Reported static sizes must be equal" n n')

let check_dynamic ~__POS__:pos typ expected v =
  Fmt.pr "Testing type: %a@." T.pp_ty typ;
  let unexpected fmt =
    Alcotest.failf ~pos
      ("Expected type to have dynamic size, but " ^^ fmt ^^ " was received.")
  in
  match T.Size.(of_value typ, of_encoding typ) with
  | Unknown, _ | _, Unknown -> unexpected "Unknown"
  | Static n, _ | _, Static n -> unexpected "Static %d" n
  | Dynamic encode, Dynamic decode ->
      Alcotest.(check ~pos int) "Expected dynamic size" expected (encode v);

      (* Check that the encoding actually occupies [n] bytes *)
      let encoding = encode_bin typ v in
      let actual_size = String.length encoding in
      Alcotest.(check ~pos int)
        "Actual size must match dynamic spec" expected actual_size;

      (* Check that the size is correctly recovered from the encoding, even after
         adding some random surrounding context. *)
      let left_pad = 1 in
      let right_pad = 0 in
      let wrapped_encoding =
        random_string left_pad ^ encoding ^ random_string right_pad
      in
      let recovered_length = decode wrapped_encoding left_pad in
      Fmt.epr "wrapped_encoding (left %d, right %d): %a@." left_pad right_pad
        Fmt.(Dump.list (fun ppf x -> Fmt.pf ppf "%d" x))
        (String.to_seq wrapped_encoding |> List.of_seq |> List.map Char.code);
      Alcotest.(check ~pos int)
        "Recovered length must match dynamic spec" expected recovered_length

let test_primitive () =
  check_static ~__POS__ T.unit 0 ();
  check_static ~__POS__ T.bool 1 true;
  check_static ~__POS__ T.char 1 ' ';
  check_static ~__POS__ T.int32 4 1l;
  check_static ~__POS__ T.int63 8 Optint.Int63.zero;
  check_static ~__POS__ T.int64 8 (-1L);
  check_static ~__POS__ T.float 8 Float.nan

let test_int () =
  let test_cases =
    (* Test a range of integers that fit correctly on this platform *)
    [
      (__POS__, 7);
      (__POS__, 14);
      (__POS__, 21);
      (__POS__, 28);
      (__POS__, 35);
      (__POS__, 42);
      (__POS__, 49);
      (__POS__, 56);
    ]
    |> List.filter (fun (_, i) -> i < Sys.int_size)
    |> List.map (fun (p, i) -> (p, 1 lsl i))
  in
  ListLabels.iteri test_cases ~f:(fun i (pos, p) ->
      check_dynamic ~__POS__:pos T.int (i + 1) (p - 1);
      check_dynamic ~__POS__:pos T.int (i + 2) p)

let test_container () =
  let module X = struct
    type two = bool * bool [@@deriving repr]
    type three = bool * bool * bool [@@deriving repr]

    let two = (true, true)
    let three = (true, true, true)
    let thirty = List.init 10 (fun _ -> three)
    let thirty_t = T.(list ~len:(`Fixed 10) three_t)
  end in
  let open X in
  check_static ~__POS__ two_t 2 two;
  check_static ~__POS__ three_t 3 three;
  check_static ~__POS__ thirty_t (3 * 10) thirty;
  check_static ~__POS__ [%typ: char * int32 * int64] (1 + 4 + 8) ('1', 4l, 8L);

  (* Option with statically sized elements *)
  check_dynamic ~__POS__ [%typ: bool option list]
    (1 + (2 + 1 + 2))
    [ Some true; None; Some false ];

  (* Option with dynamically sized elements *)
  check_dynamic ~__POS__ [%typ: int option list]
    (1 + (2 + 1 + 3 + 1 + 4))
    [ Some 1; None; Some (1 lsl 7); None; Some (1 lsl 14) ]

let test_variant () =
  let module X = struct
    type enum = A | B | C [@@deriving repr]
    type enum' = A | B of unit [@@deriving repr]
    type equal_size = A of bool | B of char [@@deriving repr]

    type mixed = Argless | Unit of unit | Char of char | Int of int
    [@@deriving repr]
  end in
  let open X in
  check_static ~__POS__ enum_t 1 A;
  check_static ~__POS__ enum'_t 1 (B ());
  check_static ~__POS__ equal_size_t 2 (A true);
  check_static ~__POS__ [%typ: unit option] 1 None;

  check_dynamic ~__POS__ mixed_t 1 Argless

let test_recursive () =
  let module X = struct
    type int_list = [] | ( :: ) of int * int_list [@@deriving repr]

    type int_tree = Leaf of int | Branch of int_tree * int_tree
    [@@deriving repr]

    type odd = S of even

    and even = Z | S' of odd [@@deriving repr]
  end in
  let open X in
  check_dynamic ~__POS__ int_list_t 1 [];
  check_dynamic ~__POS__ int_list_t 7 [ 1; 2; 3 ];

  let leaf_size = 2 (* tag + short int *) in
  let branch_size = 1 (* tag, excluding subterms *) in
  check_dynamic ~__POS__ int_tree_t leaf_size (Leaf 0);
  check_dynamic ~__POS__ int_tree_t
    (branch_size + (2 * leaf_size))
    (Branch (Leaf 1, Leaf 2));
  check_dynamic ~__POS__ int_tree_t
    ((3 * branch_size) + (4 * leaf_size))
    (Branch (Branch (Leaf 1, Leaf 2), Branch (Leaf 3, Leaf 4)));

  check_dynamic ~__POS__ even_t 1 Z;
  check_dynamic ~__POS__ odd_t 2 (S Z);
  check_dynamic ~__POS__ even_t 3 (S' (S Z));
  check_dynamic ~__POS__ odd_t 4 (S (S' (S Z)));

  let faux_recursive_t = T.(mu (fun _ -> char)) in
  check_static ~__POS__ faux_recursive_t 1 'a'

let test_unknown () =
  let module X = struct
    type opaque = Opaque [@@deriving repr]

    let opaque_t =
      let encode_bin = T.(unstage @@ encode_bin opaque_t) in
      let decode_bin = T.(unstage @@ decode_bin opaque_t) in
      let size_of = T.Size.custom_dynamic () in
      T.like ~bin:(encode_bin, decode_bin, size_of) opaque_t

    type int_list = Cons of int * int_list | Nil of opaque [@@deriving repr]
  end in
  let open X in
  check_unknown ~__POS__ opaque_t;
  check_unknown ~__POS__ int_list_t;
  ()

let tests =
  [
    ("primitive", `Quick, test_primitive);
    ("int", `Quick, test_int);
    ("container", `Quick, test_container);
    ("variant", `Quick, test_variant);
    ("recursive", `Quick, test_recursive);
    ("unknown", `Quick, test_unknown);
  ]
