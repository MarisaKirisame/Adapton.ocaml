(** Adapton self-adjusting values, alternative APIs, and applications. *)

(** Adapton with a functor-based API. *)
include AdaptonZoo.Adapton

(** Adapton with a polymorphic API. *)
module PolySA = AdaptonUtil.PolySA.Make (AdaptonZoo.Adapton)

(** Adapton with a basic polymorphic API. *)
module BasicSA = AdaptonUtil.BasicSA.Make (AdaptonZoo.Adapton)

(** Adapton self-adjusting lists. *)
module SAList = AdaptonUtil.SAList.Make (AdaptonZoo.Adapton)
