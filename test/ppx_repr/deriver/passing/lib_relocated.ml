module Elsewhere : sig
  module Foo : module type of Repr.Type

  type t [@@deriving repr { lib = Some "Foo" }]
end = struct
  module Foo = Repr.Type

  module Irmin = struct end

  type t = unit * unit [@@deriving repr { lib = Some "Foo" }]
end

module Locally_avaliable : sig
  type 'a ty

  type t [@@deriving repr { lib = None }]
end = struct
  let pair, unit = Repr.Type.(pair, unit)

  type 'a ty = 'a Repr.Type.ty

  module Irmin = struct end

  type t = unit * unit [@@deriving repr { lib = None }]
end
