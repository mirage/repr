module T = Repr

module Structural = struct
  type tuples = string * int * bool list [@@deriving repr]
end

let ( >> ) f g x = g (f x)

let shape_to_string typ =
  T.Binary_shape.of_type typ |> Fmt.to_to_string T.Binary_shape.pp_dump

let test_basic =
  shape_to_string Structural.tuples_t |> Alcotest.(check string) "Foo" ""

let tests = [ ("test_basic", test_basic) ]
