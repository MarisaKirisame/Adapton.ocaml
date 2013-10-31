
type 'a behavior

(* Core combinators. *)
val const : (module Hashtbl.SeededHashedType with type t = 'a ) -> 'a -> 'a behavior
val app : ('a->'b) behavior -> (module Hashtbl.SeededHashedType with type t = 'b ) -> 'a behavior -> 'b behavior
(* val flatten? *)
val prev : 'a behavior -> 'a -> 'a behavior
(* How do I implement this?
val fix : ('a behavior -> 'a behavior) -> 'a behavior
*)

(* Derived combinators. *)
val lift : ('a -> 'b) -> (module Hashtbl.SeededHashedType with type t = 'b ) -> 'a behavior -> 'b behavior
val lift2 : ('a -> 'b -> 'c) -> (module Hashtbl.SeededHashedType with type t = 'c ) -> 'a behavior -> 'b behavior-> 'c behavior
val lift3 : ('a -> 'b -> 'c -> 'd) -> (module Hashtbl.SeededHashedType with type t = 'd ) -> 'a behavior -> 'b behavior-> 'c behavior -> 'd behavior
val lift4 : ('a -> 'b -> 'c -> 'd -> 'e) -> (module Hashtbl.SeededHashedType with type t = 'e ) -> 'a behavior -> 'b behavior-> 'c behavior -> 'd behavior -> 'e behavior
(*
(* val appf? implement without flatten? *)

val filter : ('a -> bool) -> 'a behavior -> 'a behavior
val merge : 'a behavior -> 'a behavior -> 'a behavior

TODO: more...?
*)

(* Extractors. *)
val force : 'a behavior -> 'a

