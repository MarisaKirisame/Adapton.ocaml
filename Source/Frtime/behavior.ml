
open Adapton.Signatures
module T = Adapton.Types

(* Make a behavior, given a SAType. *)
module Make (M : SAType) : Behavior = struct
	open Time

	type 'a sa_mod = (module SAType.S with type sa = M.sa and type data = 'a and type t = 'a M.thunk)
	type 'a behavior = 'a sa_mod * 'a M.thunk * time

	let const (type t) (module H : Hashtbl.SeededHashedType with type t = t) (c : t) : t behavior = 
		let module R = M.Make( H) in
		let r = R.const c in
		let t = get_time () in
		(module R), r, t
	
	let app (type a) (type b) (((module F), f, tf) : (a -> b) behavior) (module B : Hashtbl.SeededHashedType with type t = b) (((module A), a, ta) : a behavior) : b behavior = 
		let module R = M.Make( B) in
		let r = R.thunk (fun () -> (F.force f) (A.force a)) in
		let t = min_time [ tf; ta] in
		(module R), r, t
	
	(* Contains the previous value of the behavior. Takes on the default value until the behavior changes. *)
	let prev (type a) (((module A), a, ta) : a behavior) (default : a) : a behavior =
		let store = ref (default, get_time ()) in
		let r = A.thunk (fun () ->
			let r', t' = !store in
			store := (A.force a, ta);
			r'
		)
		in
		let t = snd !store in
		(module A), r, t

	let lift (type a) (type b) (f : a -> b) (module B : Hashtbl.SeededHashedType with type t = b) (a : a behavior) : b behavior =
		let mF = T.makeFunction () in
		let f' = const mF f in
		app f' (module B) a

	let lift2 (type a) (type b) (type c) (f : a -> b -> c) (module C : Hashtbl.SeededHashedType with type t = c) (a : a behavior) (b : b behavior) : c behavior =
		let mF = T.makeFunction () in
		let f' = lift f mF a in
		app f' (module C) b
	
	let lift3 (type a) (type b) (type c) (type d) (f : a -> b -> c -> d) (module D : Hashtbl.SeededHashedType with type t = d) (a : a behavior) (b : b behavior) (c : c behavior) : d behavior =
		let mF = T.makeFunction () in
		let f' = lift2 f mF a b in
		app f' (module D) c
	
	let lift4 (type a) (type b) (type c) (type d) (type e) (f : a -> b -> c -> d -> e) (module E : Hashtbl.SeededHashedType with type t = e) (a : a behavior) (b : b behavior) (c : c behavior) (d : d behavior) : e behavior =
		let mF = T.makeFunction () in
		let f' = lift3 f mF a b c in
		app f' (module E) d

	let force (type a) (((module A), a, _) : a behavior) : a =
		A.force a
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
