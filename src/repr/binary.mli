(** This module provides functions for interacting with Repr's binary
    serialisation format directly (without first constructing a representation
    of the type being encoded). These can be useful for performance-critical
    applications, where the runtime overhead of the dynamic specialisation is
    too large, or when the actual codec being used is too complex to be
    expressed via a type representation. *)

include Binary_intf.Intf
(** @inline *)
