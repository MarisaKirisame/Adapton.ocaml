(** Default modules for self-adjusting values and applications. *)

(** Default module for self-adjusting values. *)
module SA = LazySANaive

(** Default module for self-adjusting lists. *)
module SAList = SAList.Make (SA)
