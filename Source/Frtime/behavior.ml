
open Adapton.Signatures
open Time
module T = Adapton.Types

module type Behavior = sig
	type 'a behavior

	(* Core combinators. *)
	val const : (module Hashtbl.SeededHashedType with type t = 'a ) -> 'a -> 'a behavior
	(*val memo_const : (module Hashtbl.SeededHashedType with type t = 'a ) -> 'a -> 'a behavior*)
	val app : ('a->'b) behavior -> (module Hashtbl.SeededHashedType with type t = 'b ) -> 'a behavior -> 'b behavior
	val memo_app : ('a->'b) behavior -> (module Hashtbl.SeededHashedType with type t = 'b ) -> 'a behavior -> 'b behavior
	(* TODO:val memo_app ... *)
	(* val flatten? *)
	(* val prev : 'a behavior -> 'a -> 'a behavior *)
	(* How do I implement this?
	val fix : ('a behavior -> 'a behavior) -> 'a behavior
	*)

	(*
	(* val appf? implement without flatten? *)

	val filter : ('a -> bool) -> 'a behavior -> 'a behavior
	when

	TODO: 
		memo_app
		ifb -> switch? ifb (thunkified)

	
	more...?
		? update_const/trigger ?




	*)
	val merge : 'a behavior -> 'a behavior -> 'a behavior
	val ifb : bool behavior -> 'a behavior -> 'a behavior -> 'a behavior

	(* Derived combinators. *)
	val lift : ('a -> 'b) -> (module Hashtbl.SeededHashedType with type t = 'b ) -> 'a behavior -> 'b behavior
	val lift2 : ('a -> 'b -> 'c) -> (module Hashtbl.SeededHashedType with type t = 'c ) -> 'a behavior -> 'b behavior-> 'c behavior
	val lift3 : ('a -> 'b -> 'c -> 'd) -> (module Hashtbl.SeededHashedType with type t = 'd ) -> 'a behavior -> 'b behavior-> 'c behavior -> 'd behavior
	val lift4 : ('a -> 'b -> 'c -> 'd -> 'e) -> (module Hashtbl.SeededHashedType with type t = 'e ) -> 'a behavior -> 'b behavior-> 'c behavior -> 'd behavior -> 'e behavior

	val memo_lift : ('a -> 'b) -> (module Hashtbl.SeededHashedType with type t = 'b ) -> 'a behavior -> 'b behavior
	val memo_lift2 : ('a -> 'b -> 'c) -> (module Hashtbl.SeededHashedType with type t = 'c ) -> 'a behavior -> 'b behavior-> 'c behavior
	val memo_lift3 : ('a -> 'b -> 'c -> 'd) -> (module Hashtbl.SeededHashedType with type t = 'd ) -> 'a behavior -> 'b behavior-> 'c behavior -> 'd behavior
	val memo_lift4 : ('a -> 'b -> 'c -> 'd -> 'e) -> (module Hashtbl.SeededHashedType with type t = 'e ) -> 'a behavior -> 'b behavior-> 'c behavior -> 'd behavior -> 'e behavior
	
	(* Alarm. *)
	val seconds : unit -> time behavior
	val time : unit -> time behavior

	(* Extractors. *)
	val force : 'a behavior -> 'a
	val id : 'a behavior -> int
end

(* Make a behavior, given a SAType. *)
module Make (M : SAType) : Behavior = struct
	module Tm = TimeType(M)
	module S = Sys
	module U = Unix

	type 'a sa_mod = (module SAType.S with type sa = M.sa and type data = 'a and type t = 'a M.thunk)
	type 'a behavior = 'a sa_mod * 'a M.thunk * time M.thunk
	(* Requirement: Always force 'a thunk before time thunk. *)

	let const (type t) (module H : Hashtbl.SeededHashedType with type t = t) (c : t) : t behavior = 
		let module R = M.Make( H) in
		let r = R.const c in
		let t = Tm.const (get_time ()) in
		(module R), r, t

	(*
	(* Universal type hackery. 
	https://ocaml.janestreet.com/?q=node/18
	*)
	module Univ : sig
		type t
		val embed: unit -> ('a -> t) * (t -> 'a option)
	end = struct 
		type t = exn

		let embed (type s) () =
			let module M = struct exception E of s end in
			(fun x -> M.E x), (function M.E x -> Some x | _ -> None)
	end
	
	let memo_const_store = Mixtbl.create 10(*ref []*) (*Hashtbl.create 10*)
	let memo_const (type t) (module H : Hashtbl.SeededHashedType with type t = t) (c : t) : t behavior = 
		(*
		let of1, to1 = Univ.embed () in
		let of2, to2 = Univ.embed () in
		let i = ref (of1 13) in
		assert( to1 !i = Some 13);
		assert( to2 !i = Some 13);
		*)
		let module R = M.Make( H) in
		let memo = 
			let get, set = Mixtbl.access () in
			match get memo_const_store c with
			| Some m -> 
				m
			| None ->
				let m = R.memo (module R.Data) (fun _ c' -> c') in
				set memo_const_store c m;
				m
		in
		(*
		let memo = 
			let (of_memo_t, to_memo_t) = Univ.embed () in
			let rec helper = function
			| [] ->
				let m = R.memo (module R.Data) (fun _ c' -> c') in
				let m' = ref (of_memo_t m) in
				memo_const_store := m'::!memo_const_store;
				Printf.printf "memo_const: making new..\n%!";
				m
			| h::t ->
				let m = to_memo_t !h in
				match m with
				| None ->
					Printf.printf "memo_const: not this one\n%!";
					helper t
				| Some m' ->
					Printf.printf "memo_const: found!\n%!";
					m'
			in
			helper !memo_const_store
		in	
		let memo = 
			let mod_r = (module R : SAType.S with type sa = M.sa and type data = t and type t = t M.thunk) in
			try
				Hashtbl.find memo_const_store mod_r
			with
			| Not_found -> 
				let m = R.memo (module R.Data) (fun _ c' -> c') in
				Hashtbl.add memo_const_store mod_r m
		in
		*)
		(*let r = memo c in*)
		let t = Tm.const (get_time ()) in
		(*fun (c : t) ->*)
			(module R), memo c, t
	*)
		
	let app (type a) (type b) (((module F), f, tf) : (a -> b) behavior) (module B : Hashtbl.SeededHashedType with type t = b) (((module A), a, ta) : a behavior) : b behavior = 
		let module R = M.Make( B) in
		let r = R.thunk (fun () -> (F.force f) (A.force a)) in
		let t = Tm.thunk (fun () -> max_time [ Tm.force tf; Tm.force ta]) in
		(module R), r, t
	
	let memo_app (type a) (type b) (((module F), f, tf) : (a -> b) behavior) (module B : Hashtbl.SeededHashedType with type t = b) (((module A), a, ta) : a behavior) : b behavior = 
		let module R = M.Make( B) in
		let memo = R.memo2 (module F.Data) (module A.Data) (fun _ f' a' -> f' a') in
		let r = R.thunk (fun () -> R.force (memo (F.force f) (A.force a))) in
		let t = Tm.thunk (fun () -> max_time [ Tm.force tf; Tm.force ta]) in
		(module R), r, t

		
	(* Contains the previous value of the behavior. Takes on the default value until the behavior changes. *)
	(* TODO: Not pure... How do we fix this?
	let prev (type a) (((module A), a, ta) : a behavior) (default : a) : a behavior =
		(* TODO: Combine stores? Otherwise, might lead to race conditions?
		Need to keep track of times, to see if force causes store to update.
		let store = ref (default, get_time ()) in
		*)
		let valStore = ref default in
		let tmStore = ref (get_time ()) in (* default time should time prev was called? *)
		let r = A.thunk (fun () ->
			let r' = !valStore in
			valStore := A.force a;
			r'
		)
		in
		let t = Tm.thunk (fun () ->
			let t' = !tmStore in
			tmStore := Tm.force ta;
			t'
		)
		in
		(module A), r, t
	*)

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

	let memo_lift (type a) (type b) (f : a -> b) (module B : Hashtbl.SeededHashedType with type t = b) ((module A), a, ta : a behavior) : b behavior =
		let module R = M.Make( B) in
		let memo = R.memo (module A.Data) (fun _ -> f) in
		let r = R.thunk (fun () -> R.force (memo (A.force a))) in
		let t' = Tm.const (get_time ()) in
		let t = Tm.thunk (fun () -> max_time [ Tm.force t'; Tm.force ta]) in
		(module R), r, t
		
	(* Can't be compositional here because memoizing on closures does not work. *)
	let memo_lift2 (type a) (type b) (type c) (f : a -> b -> c) (module C : Hashtbl.SeededHashedType with type t = c) ((module A), a, ta : a behavior) ((module B), b, tb : b behavior) : c behavior =
		let module R = M.Make( C) in
		let memo = R.memo2 (module A.Data) (module B.Data) (fun _ -> f) in
		let r = R.thunk (fun () -> R.force (memo (A.force a) (B.force b))) in
		let t' = Tm.const (get_time ()) in
		let t = Tm.thunk (fun () -> max_time [ Tm.force t'; Tm.force ta; Tm.force tb]) in
		(module R), r, t
		(*
		let mF = T.makeFunction () in
		let f' = memo_lift f mF a in
		memo_app f' (module C) b
		*)

	let memo_lift3 (type a) (type b) (type c) (type d) (f : a -> b -> c -> d) (module D : Hashtbl.SeededHashedType with type t = d) ((module A), a, ta : a behavior) ((module B), b, tb : b behavior) ((module C), c, tc : c behavior) : d behavior =
		let module R = M.Make( D) in
		let memo = R.memo3 (module A.Data) (module B.Data) (module C.Data) (fun _ -> f) in
		let r = R.thunk (fun () -> R.force (memo (A.force a) (B.force b) (C.force c))) in
		let t' = Tm.const (get_time ()) in
		let t = Tm.thunk (fun () -> max_time [ Tm.force t'; Tm.force ta; Tm.force tb; Tm.force tc]) in
		(module R), r, t

	let memo_lift4 (type a) (type b) (type c) (type d) (type e) (f : a -> b -> c -> d -> e) (module E : Hashtbl.SeededHashedType with type t = e) ((module A), a, ta : a behavior) ((module B), b, tb : b behavior) ((module C), c, tc : c behavior) ((module D), d, td : d behavior) : e behavior =
		let module R = M.Make( E) in
		let memo = R.memo4 (module A.Data) (module B.Data) (module C.Data) (module D.Data) (fun _ -> f) in
		let r = R.thunk (fun () -> R.force (memo (A.force a) (B.force b) (C.force c) (D.force d))) in
		let t' = Tm.const (get_time ()) in
		let t = Tm.thunk (fun () -> max_time [ Tm.force t'; Tm.force ta; Tm.force tb; Tm.force tc; Tm.force td]) in
		(module R), r, t

	(* Take on the value of the most recently updated behavior. *)
	let merge (type a) (((module A), a, ta) : a behavior) (( _, b, tb) : a behavior) : a behavior =
		(* Precompute force to satisfy requirement and maintain consistency. *)
		ignore (A.force a);
		ignore (A.force b);
		let t = Tm.const (max_time [ Tm.force ta; Tm.force tb]) in
		let r = A.thunk (fun () ->
			let ta' = Tm.force ta in
			let tb' = Tm.force tb in
			A.force begin
				(* ta < tb -> !(ta >= tb) *)
				if cmp_time ta' tb' then (
					Tm.update_const t tb';
					b
				) else (
					Tm.update_const t ta';
					a
				)
			end
		)
		in
		(*let t = Tm.thunk (fun () -> max_time [ Tm.force ta; Tm.force tb]) in (* TODO: switch to a tmStore? *)*)
		(module A), r, t

	(*
	(* Only pass on events that satisfy the predicate. add a default? *)
	let filter (type a) (pred : a -> bool) (((module A), a, ta) : a behavior) : a behavior =
		let t = Tm.const (Tm.force ta) in
		let r = A.thunk (fun () ->
			let a' = A.force a in
			if pred a' then
				Tm.update_const t (Tm.force ta);
				a'
			else
				...
		)
		in
		(module A), r, t
	*)

	let ifb (type a) (((module G), g, tg) : bool behavior) (((module A), a, ta) : a behavior) ((_, b, tb) : a behavior) : a behavior = 
		let r = A.thunk (fun () ->
			A.force begin
				if G.force g then
					a
				else
					b
			end
		)
		in
		let t = Tm.thunk (fun () -> max_time [ Tm.force tg; Tm.force ta; Tm.force tb]) in
		(module A), r, t

	let seconds_store = ref None
	let seconds () : time behavior = 
		let c = !seconds_store in
		match c with
		| None ->
			(* Can use a store since the value behaviors is forced first and there is not past dependence. *)
			let store = ref (from_seconds 0.0) in
			let t = Tm.thunk (fun () -> !store) in
			let r = Tm.const (get_time ()) in
			let handle = S.Signal_handle (fun s ->
				if s <> S.sigalrm then (
					Printf.printf "Error: FRP seconds handler called for non alarm signal.\n";
					()
				) else (
					let t' = get_time () in
					store := t';
					Tm.update_const r t';
					ignore (U.alarm 1);
					()
				)
			)
			in
			S.set_signal S.sigalrm handle;
			ignore (U.alarm 1);
			let beh = (module Tm : SAType.S with type sa = M.sa and type data = time and type t = time M.thunk), r, t in
			seconds_store := Some beh;
			beh
		| Some c' ->
			c'

	(* Time store used to only create one time behavior. *)
	let time_store = ref None
	let time () = 
		match !time_store with
		| None ->
			(* Can use a store since the value behaviors is forced first and there is not past dependence. *)
			let store = ref (from_seconds 0.0) in
			let t = Tm.thunk (fun () -> !store) in
			let r = Tm.thunk (fun () ->
				let t' = get_time () in
				store := t';
				t'
			)
			in
			let beh = (module Tm : SAType.S with type sa = M.sa and type data = time and type t = time M.thunk), r, t in
			time_store := Some beh;
			beh
		| Some c ->
			c

	let force (type a) (((module A), a, _) : a behavior) : a =
		A.force a
	
	(* Returns the id of the underlying thunk. *)
	let id (type a) ((module A), a, _ : a behavior) : int = 
		A.id a
end

