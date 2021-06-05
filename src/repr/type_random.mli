open Type_core
open Staging
module Attr : Attribute.S1 with type 'a t = Random.State.t -> 'a

val of_state : 'a t -> (Random.State.t -> 'a) staged
val of_global : 'a t -> (unit -> 'a) staged
