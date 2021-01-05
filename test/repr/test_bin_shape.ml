module T = Repr

module Structural = struct
  type tuples = string * int * bool list [@@deriving repr]
  type tree = Branch of int * tree * tree | Leaf [@@deriving repr]
end

let ( >> ) f g x = g (f x)

let shape_to_string typ =
  T.Binary_shape.of_type typ |> Fmt.to_to_string (T.pp_dump T.Binary_shape.t)

let test_basic () =
  let () =
    shape_to_string Structural.tuples_t
    |> Alcotest.(check string)
         "Foo"
         "Triple ((Contiguous ((Boxed (Int), Char)), Int,\n\
         \         Contiguous ((Boxed (Int), Bool))))"
  in
  let () =
    shape_to_string Structural.tree_t
    |> Alcotest.(check string)
         "Foo"
         {|Recursive (Variant ([("Branch", Triple ((Int, Var (1), Var (1))));
                     ("Leaf", Empty)]))|}
  in
  ()

let tests = [ Alcotest.test_case "test_basic" `Quick test_basic ]
