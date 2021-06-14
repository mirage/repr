module Alcotest = struct
  include Alcotest

  let gcheck ?pos typ msg a b =
    let equal = Repr.(unstage (equal typ)) in
    let pp = Repr.pp_dump typ in
    check ?pos (testable pp equal) msg a b

  let string = testable Fmt.Dump.string String.equal
end
