
open Frtime

module T = Adapton.Types
module B = Behavior.Make( Adapton.LazySABidi)
module Tm = Time

let t = B.seconds ()
let t0 = Tm.to_seconds (B.force t)
let ellapsed = B.lift (fun t -> int_of_float ((Tm.to_seconds t) -. t0)) (module T.Int) t
let print_time () =
	Printf.printf "seconds: %d\n%!" (B.force ellapsed)

let oddGuard = B.lift (fun i -> i mod 2 == 1) (module T.Bool) ellapsed
let oddCase = B.const (T.makeFunction ()) (fun i -> Printf.printf "seconds: %d\n%!" i)
let evenCase = B.const (T.makeFunction ()) (fun i -> Printf.printf "even!\n%!")
let ifb = B.ifb oddGuard oddCase evenCase
let branched = B.app ifb (module T.Unit) ellapsed

let rec loop i = 
	(if false then
		print_time ()
	else
		ignore (B.force branched)
	);
	(*Unix.sleep 1;*)
	loop (i+1)

let testUniv () = 
	let id x = x in
	(*let module F = T.makeFunction () in*)
	assert (B.memo_const (module T.Bool) true == B.memo_const (module T.Bool) true);
	assert (B.memo_const (T.makeFunction ()) id == B.memo_const (T.makeFunction ()) id);
	assert (B.memo_const (T.makeFunction ()) id != B.memo_const (T.makeFunction ()) (fun x -> x));
	()

let _ = 
	assert (oddCase != evenCase);
	assert (evenCase == evenCase);
	testUniv ();
	loop 0

