
open Adapton.Signatures
module T = Adapton.Types
module type SHT = Hashtbl.SeededHashedType

(* TODO: temp. make functor version in types.ml *)
let makeFunction (type a) (type b) (f : a -> b) : (module SHT with type t = a -> b) = 
	let module F = struct
		type t = a -> b
		let equal = (==)
		let hash = Hashtbl.seeded_hash
	end
	in 
	(module F)

(* TODO: I'm not sure whether this'll work in general... *)
let makeFunctionReturn (type b) (type c) (f : 'a -> 'b -> 'c) : (module SHT with type t = b -> c) = 
	let module F = struct
		type t = b -> c
		let equal = (==)
		let hash = Hashtbl.seeded_hash
	end
	in 
	(module F)

module type Behavior = sig
	type 'a behavior

	(* Core combinators. *)
	val const : (module SHT with type t = 'a ) -> 'a -> 'a behavior
	val app : ('a->'b) behavior -> (module SHT with type t = 'b ) -> 'a behavior -> 'b behavior
	(* val flatten? *)
	val prev : 'a behavior -> 'a -> 'a behavior
	(* How do I implement this?
	val fix : ('a behavior -> 'a behavior) -> 'a behavior
	*)

	(* Derived combinators. *)
	val lift : ('a -> 'b) -> (module SHT with type t = 'b ) -> 'a behavior -> 'b behavior
	val lift2 : ('a -> 'b -> 'c) -> (module SHT with type t = 'c ) -> 'a behavior -> 'b behavior-> 'c behavior
	val lift3 : ('a -> 'b -> 'c -> 'd) -> (module SHT with type t = 'd ) -> 'a behavior -> 'b behavior-> 'c behavior -> 'd behavior
	val lift4 : ('a -> 'b -> 'c -> 'd -> 'e) -> (module SHT with type t = 'e ) -> 'a behavior -> 'b behavior-> 'c behavior -> 'd behavior -> 'e behavior
	(*
	(* val appf? implement without flatten? *)

	val filter : ('a -> bool) -> 'a behavior -> 'a behavior
	val merge : 'a behavior -> 'a behavior -> 'a behavior

	TODO: more...?
	*)
end

(* Make a behavior, given a SAType. *)
module Make (M : SAType) : Behavior = struct
	type 'a sa_mod = (module SAType.S with type sa = M.sa and type data = 'a and type t = 'a M.thunk)
	type 'a behavior = 'a sa_mod * 'a M.thunk (* Add a time component of event that propogates? *)

	let const (type t) (module H : SHT with type t = t) (c : t) : t behavior = 
		let module R = M.Make( H) in
		let r = R.const c in
		(module R), r
	
	let app (type a) (type b) (((module F), f) : (a -> b) behavior) (module B : SHT with type t = b) (((module A), a) : a behavior) : b behavior = 
		let module R = M.Make( B) in
		let r = R.thunk (fun () -> (F.force f) (A.force a)) in
		(module R), r
	
	let prev (type a) (((module A), a) : a behavior) (default : a) : a behavior =
		let store = ref default in
		let r = A.thunk (fun () ->
			let r' = !store in
			store := A.force a;
			r'
		)
		in
		(module A), r

	let lift (type a) (type b) (f : a -> b) (module B : SHT with type t = b) (a : a behavior) : b behavior = 
		let mF = makeFunction f in
		let f' = const mF f in
		app f' (module B) a

	let lift2 (type a) (type b) (type c) (f : a -> b -> c) (module C : SHT with type t = c) (a : a behavior) (b : b behavior) : c behavior =
		let mF = makeFunctionReturn f in
		let f' = lift f mF a in
		app f' (module C) b
	
	let lift3 (type a) (type b) (type c) (type d) (f : a -> b -> c -> d) (module D : SHT with type t = d) (a : a behavior) (b : b behavior) (c : c behavior) : d behavior =
		let mF = makeFunctionReturn f in
		let f' = lift2 f mF a b in
		app f' (module D) c
	
	let lift4 (type a) (type b) (type c) (type d) (type e) (f : a -> b -> c -> d -> e) (module E : SHT with type t = e) (a : a behavior) (b : b behavior) (c : c behavior) (d : d behavior) : e behavior = 
		let mF = makeFunctionReturn f in
		let f' = lift3 f mF a b c in
		app f' (module E) d

end

(*
module type Test = sig
	type t
	val eq : t -> t -> bool
end

module TestA = struct type t = int  let eq x y = x = y end
module TestB = struct type t = float let eq x y = x = y end


(*
type 'b behavior = (module SA with type sa = ? and type 'a thunk = ?) * ?
*)

(*let const (type t) (module M : Test with type t = t) (v:t) = v*)
(*let const (type t) (module M : Test with type t = t) (v:t) = (module M)*)
let const (type t) (module H : SeededHashedType with type t = t) (v:t) : t behavior = (module M : Test with type t = t), v

let i = const (module TestA) 4
let f = const (module TestB) 4.4

let b_eq (type a) (x:a behavior) (y:a behavior) = 
	let (module A) = (fst x) in
	A.eq (snd x) (snd y) 
module type HSHT = Hashtbl.SeededHashedType

class ['a] behavior = object
	val m : (module SAType.S with type t = 'a)
	val v : m.t thunk
end

let const (module C : HSHT) c = (*(module C : HSHT) * c*)
	let (module M : )
	
*)
