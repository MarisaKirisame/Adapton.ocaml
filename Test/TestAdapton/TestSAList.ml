open TestUtil.MyOUnit
open Format

module Int = struct
    type t = int
    let hash x = x
    let equal = (==)
end
let assert_int_equal = assert_equal ~printer:pp_print_int
let assert_list_equal = assert_equal ~printer:(list_printer pp_print_int ",")

let make_regression_testsuite (module L : Adapton.Signatures.SAListType) =
    let module I = L.Make (Int) in

    "Regression" >::: [
        "filter" >:: begin fun () ->
            let pred = (<) 1 in

            let xs = [ 1; 2; 3 ] in
            let ys = List.filter pred xs in
            let zs = ys in

            let xs' = I.of_list xs in
            let ys' = I.filter pred xs' in
            let zs' = I.to_list ys' in

            assert_list_equal ~msg:"initial" zs zs';

            let xs = 4::xs in
            let ys = List.filter pred xs in
            let zs = ys in

            let ys' = try
                I.push 4 xs';
                I.refresh ();
                ys'
            with Adapton.Exceptions.NonSelfAdjustingValue ->
                let xs' = I.of_list xs in
                let ys' = I.filter pred xs' in
                ys'
            in
            let zs' = I.to_list ys' in

            assert_list_equal ~msg:"update" zs zs';
        end;

        "append" >:: begin fun () ->
            let xs = [ 1; 2; 3 ] in
            let ys = List.append xs xs in
            let zs = ys in

            let xs' = I.of_list xs in
            let ys' = I.append xs' xs' in
            let zs' = I.to_list ys' in

            assert_list_equal ~msg:"initial" zs zs';

            let xs = 4::xs in
            let ys = List.append xs xs in
            let zs = ys in

            let ys' = try
                I.push 4 xs';
                I.refresh ();
                ys'
            with Adapton.Exceptions.NonSelfAdjustingValue ->
                let xs' = I.of_list xs in
                let ys' = I.append xs' xs' in
                ys'
            in
            let zs' = I.to_list ys' in

            assert_list_equal ~msg:"update" zs zs';
        end;

        "map" >:: begin fun () ->
            let fn = succ in

            let xs = [ 1; 2; 3 ] in
            let ys = List.map fn xs in
            let zs = ys in

            let xs' = I.of_list xs in
            let ys' = I.map (module I) fn xs' in
            let zs' = I.to_list ys' in

            assert_list_equal ~msg:"initial" zs zs';

            let xs = 4::xs in
            let ys = List.map fn xs in
            let zs = ys in

            let ys' = try
                I.push 4 xs';
                I.refresh ();
                ys'
            with Adapton.Exceptions.NonSelfAdjustingValue ->
                let xs' = I.of_list xs in
                let ys' = I.map (module I) fn xs' in
                ys'
            in
            let zs' = I.to_list ys' in

            assert_list_equal ~msg:"update" zs zs';
        end;

        "filter map" >:: begin fun () ->
            let pred = (<) 1 in
            let fn = succ in

            let ws = [ 1; 2; 3 ] in
            let xs = List.filter pred ws in
            let ys = List.map fn xs in
            let zs = ys in

            let ws' = I.of_list ws in
            let xs' = I.filter pred ws' in
            let ys' = I.map (module I) fn xs' in
            let zs' = I.to_list ys' in

            assert_list_equal ~msg:"initial" zs zs';

            let ws = 4::ws in
            let xs = List.filter pred ws in
            let ys = List.map fn xs in
            let zs = ys in

            let ys' = try
                I.push 4 ws';
                I.refresh ();
                ys'
            with Adapton.Exceptions.NonSelfAdjustingValue ->
                let ws' = I.of_list ws in
                let xs' = I.filter pred ws' in
                let ys' = I.map (module I) fn xs' in
                ys'
            in
            let zs' = I.to_list ys' in

            assert_list_equal ~msg:"update" zs zs';
        end;

        "filter map append" >:: begin fun () ->
            let pred = (<) 1 in
            let fn = succ in

            let vs = [ 1; 2; 3 ] in
            let ws = List.filter pred vs in
            let xs = List.map fn ws in
            let ys = List.append xs xs in
            let zs = ys in

            let vs' = I.of_list vs in
            let ws' = I.filter pred vs' in
            let xs' = I.map (module I) fn ws' in
            let ys' = I.append xs' xs' in
            let zs' = I.to_list ys' in

            assert_list_equal ~msg:"initial" zs zs';

            let vs = 4::vs in
            let ws = List.filter pred vs in
            let xs = List.map fn ws in
            let ys = List.append xs xs in
            let zs = ys in

            let ys' = try
                I.push 4 vs';
                I.refresh ();
                ys'
            with Adapton.Exceptions.NonSelfAdjustingValue ->
                let vs' = I.of_list vs in
                let ws' = I.filter pred vs' in
                let xs' = I.map (module I) fn ws' in
                let ys' = I.append xs' xs' in
                ys'
            in
            let zs' = I.to_list ys' in

            assert_list_equal ~msg:"update" zs zs';
        end;

        "memo" >:: begin fun () ->
            try
                let update_count = ref 0 in
                let pred s = incr update_count; succ s in
                let xs = I.of_list [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ] in
                I.push 0 xs; (* to detect if non-self-adjusting *)
                let ys = I.map (module I) pred xs in
                assert_int_equal ~msg:"initial" 0 !update_count;
                I.refresh ();
                ignore (I.to_list ys);
                assert_int_equal ~msg:"force" 10 !update_count;
                I.push 0 xs;
                ignore (I.to_list ys);
                assert_int_equal ~msg:"push and force" 12 !update_count; (* the first and second elements *)
                I.pop xs;
                ignore (I.to_list ys);
                assert_int_equal ~msg:"pop and force" 13 !update_count; (* the first element *)
            with Adapton.Exceptions.NonSelfAdjustingValue ->
                ()
        end;

        "gc" >:: begin fun () ->
            try
                let gc_count = ref 0 in
                let rec finalise xs =
                    Gc.finalise (fun _ -> incr gc_count) xs;
                    match I.force xs with `Cons (_, xs) -> finalise xs | `Nil -> ()
                in
                let xs = I.of_list [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 0 ] in
                let ys = I.map (module I) succ xs in
                I.refresh ();
                finalise ys;
                assert_int_equal ~msg:"initial" 0 !gc_count;
                Gc.compact ();
                assert_int_equal ~msg:"compact nothing" 0 !gc_count;
                I.update_const xs `Nil;
                assert_int_equal ~msg:"update" 0 !gc_count;
                Gc.compact ();
                assert_int_equal ~msg:"compact nothing again" 0 !gc_count;
                Gc.compact ();
                I.refresh ();
                ignore (I.to_list ys);
                (* GC shouldn't be triggered yet in OCaml's default GC policies *)
                assert_int_equal ~msg:"before compact yet again" 0 !gc_count;
                Gc.compact ();
                (* GC should collect all of ys that obsolete after update (clear the memoization tables) *)
                assert_int_equal ~msg:"compact ys" 10 !gc_count;
            with Adapton.Exceptions.NonSelfAdjustingValue ->
                ()
        end;
    ]


let make_testsuite ( name, salist ) =
    name >::: [
        make_regression_testsuite salist
    ]


let testsuite = "TestSAList" >::: List.map make_testsuite Adapton.All.salist_list
