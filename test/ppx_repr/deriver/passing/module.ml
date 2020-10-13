(* Types within modules *)
module ModuleQualifiedTypes = struct
  module X = struct
    type t = int [@@deriving repr]
  end

  module Y = struct
    type foo = X.t list [@@deriving repr]
  end

  type t = X.t [@@deriving repr]
  type t_result = (X.t, unit) result [@@deriving repr]
  type foo = Y.foo [@@deriving repr]
  type foo_list = Y.foo list [@@deriving repr]
end
