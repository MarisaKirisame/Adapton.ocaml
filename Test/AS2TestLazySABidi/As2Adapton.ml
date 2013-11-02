 module A = Adapton.PolySA.Make(Adapton.LazySABidi)
(* module A = Adapton.PolySA.Make(Adapton.EagerSATotalOrder) *)
(* module A = Adapton.PolySA.Make(Adapton.NonSAEager) *)

include A
