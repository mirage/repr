include Attribute_intf
open Higher

module Key = struct
  type 'f t = { uid : int; name : string; wit : 'f Witness.t }

  let uid =
    let counter = ref (-1) in
    fun () ->
      incr counter;
      !counter

  let create ~name =
    let uid = uid () in
    let wit = Witness.make () in
    { uid; name; wit }

  let name t = t.name

  type 'a ty = 'a t

  module Boxed = struct
    type t = E : _ ty -> t [@@ocaml.unboxed]

    let compare (E k1) (E k2) = Int.compare k1.uid k2.uid
  end
end

module Map = struct
  open Map.Make (Key.Boxed)

  type ('a, 'f) data = ('a, 'f) app
  type 'a binding = B : 'f Key.t * ('a, 'f) data -> 'a binding
  type nonrec 'a t = 'a binding t

  let empty = empty
  let is_empty = is_empty
  let mem t k = mem (E k) t
  let add t ~key ~data = add (E key) (B (key, data)) t

  let update :
      type a f.
      a t -> f Key.t -> ((a, f) data option -> (a, f) data option) -> a t =
   fun t k f ->
    update (E k)
      (fun b ->
        let v =
          f
            (match b with
            | None -> None
            | Some (B (k', v)) -> (
                match Witness.eq k.wit k'.wit with
                | None -> None
                | Some Refl -> Some v))
        in
        match v with None -> None | Some v -> Some (B (k, v)))
      t

  let singleton k v = singleton (E k) (B (k, v))
  let iter t ~f = iter (fun _ b -> f b) t
  let for_all t ~f = for_all (fun _ b -> f b) t
  let exists t ~f = exists (fun _ b -> f b) t
  let cardinal t = cardinal t
  let bindings t = bindings t |> List.map snd

  let find : type a f. a t -> f Key.t -> (a, f) data option =
   fun t k ->
    match find_opt (E k) t with
    | None -> None
    | Some (B (k', v)) -> (
        match Witness.eq k.wit k'.wit with None -> None | Some Refl -> Some v)
end

module Make1 (T : sig
  type 'a t

  val name : string
end) =
struct
  include T
  include Branded.Make (T)

  let key : br Key.t = Key.create ~name

  let find map =
    match Map.find map key with None -> None | Some x -> Some (prj x)

  let add data map = Map.add map ~key ~data:(inj data)
end

include Key

module type S1 = S1 with type 'a attr := 'a t and type 'a map := 'a Map.t
