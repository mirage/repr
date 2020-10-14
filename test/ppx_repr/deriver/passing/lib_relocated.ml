module Elsewhere : sig
  module Foo : module type of Repr

  type t [@@deriving repr { lib = Some "Foo" }]
end = struct
  module Foo = Repr
  module Irmin = struct end

  type t = unit * unit [@@deriving repr { lib = Some "Foo" }]
end

module Locally_avaliable : sig
  type 'a ty
  type t [@@deriving repr { lib = None }]
end = struct
  let pair, unit = Repr.(pair, unit)

  type 'a ty = 'a Repr.ty

  module Irmin = struct end

  type t = unit * unit [@@deriving repr { lib = None }]
end
