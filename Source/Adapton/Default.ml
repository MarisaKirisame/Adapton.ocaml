(** Default modules for self-adjusting values and applications. *)

(** Default module for self-adjusting values providing a functorized API. *)
module SA = LazySABidi

(** Default module for self-adjusting lists. *)
module SAList = SAList.Make (SA)

(** Default module for self-adjusting values providing a polymorphic API. *)
module PolySA = PolySA.Make (SA)

(** Default module for self-adjusting values providing a basic polymorphic API. *)
module BasicSA = BasicSA.Make (SA)
