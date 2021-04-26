open Type_core
open Staging

val of_state : 'a t -> (Random.State.t -> 'a) staged
val of_global : 'a t -> (unit -> 'a) staged
