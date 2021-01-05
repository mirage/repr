module T = Repr

module Alcotest = struct
  include Alcotest

  let assert_ msg = Alcotest.(check bool) msg true
end

let shape_to_string typ =
  T.Binary_shape.of_type typ |> Fmt.to_to_string T.(pp_dump Binary_shape.t)

let test_basic () =
  let module Structural = struct
    type tuples = string * int * bool list [@@deriving repr]
    type tree = Branch of int * tree * tree | Leaf [@@deriving repr]
  end in
  let () =
    shape_to_string Structural.tuples_t
    |> Alcotest.(check string)
         "Foo"
         "Product ([Contiguous ((Boxed (Int), Char)); Int;\n\
         \          Contiguous ((Boxed (Int), Bool))])"
  in
  let () =
    shape_to_string Structural.tree_t
    |> Alcotest.(check string)
         "Foo"
         {|Recursive (Variant ([Product ([Int; Var (1); Var (1)]); Empty]))|}
  in
  ()

let digest typ =
  T.Binary_shape.of_type typ |> T.(unstage (short_hash Binary_shape.t))

let test_basic_equivs () =
  Alcotest.assert_ "Strings are equivalent to bytes"
    (digest T.string = digest T.bytes);

  Alcotest.assert_ "Arrays are equivalent to lists"
    (digest T.(list int) = digest T.(array int));

  Alcotest.assert_ "Unsized arrays are not equivalent to tuples"
    (digest T.(array int) <> digest T.(pair int int));

  Alcotest.assert_ "Sized arrays _are_ equivalent to tuples"
    (digest T.(array ~len:(`Fixed 2) int) = digest T.(pair int int)
    && digest T.(array ~len:(`Fixed 3) int) = digest T.(triple int int int));

  ()

let test_algebraic_equivs () =
  Alcotest.assert_ "Changing the variant case name doesn't change the shape"
    (let module T = struct
       type uk = Colour of int * int * int [@@deriving repr]
       type us = Color of int * int * int [@@deriving repr]
     end in
    digest T.uk_t = digest T.us_t);

  Alcotest.assert_ "Records are equivalent to (non-monomorphic) tuples"
    (let module T = struct
       type a = { one : int; two : string; three : bytes } [@@deriving repr]
       type b = { uno : int; dos : string; tres : bytes } [@@deriving repr]
       type c = int * string * bytes [@@deriving repr]
     end in
    digest T.a_t = digest T.b_t && digest T.a_t = digest T.c_t);

  Alcotest.assert_ "Changing the order of variant cases changes the shape"
    (let module T = struct
       type either = Left of int | Right of string [@@deriving repr]
       type either' = Right of string | Left of int [@@deriving repr]
     end in
    digest T.either_t <> digest T.either'_t);

  ()

let tests =
  [
    Alcotest.test_case "test_basic" `Quick test_basic;
    Alcotest.test_case "algebraic_equivs" `Quick test_algebraic_equivs;
    Alcotest.test_case "basic_equivs" `Quick test_basic_equivs;
  ]
