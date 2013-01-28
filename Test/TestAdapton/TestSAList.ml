open TestUtil.MyOUnit
open Format

let assert_int_equal = assert_equal ~printer:pp_print_int
let assert_list_equal = assert_equal ~printer:(list_printer pp_print_int ",")

let make_regression_testsuite (module L : Adapton.Signatures.SAListType) =
    let module I = L.Make (Adapton.Types.Int) in

    "Correctness" >::: [
        "filter" >:: QC.forall (QC.triple (QC.list QC.int) QC.int QC.int) begin fun ( xs, x, p ) ->
            let pred = (<) p in
            let filter_pred, _ = I.memo_filter pred in

            let ys = List.filter pred xs in
            let zs = ys in

            let xs' = I.of_list xs in
            let ys' = filter_pred xs' in
            let zs' = I.to_list ys' in

            assert_list_equal ~msg:"initial" zs zs';

            let xs = x::xs in
            let ys = List.filter pred xs in
            let zs = ys in

            let ys' = try
                I.push x xs';
                I.refresh ();
                ys'
            with Adapton.Exceptions.NonSelfAdjustingValue ->
                let xs' = I.of_list xs in
                let ys' = filter_pred xs' in
                ys'
            in
            let zs' = I.to_list ys' in

            assert_list_equal ~msg:"update" zs zs';
        end;

        "append" >:: QC.forall (QC.pair (QC.list QC.int) QC.int) begin fun ( xs, x ) ->
            let append, _ = I.memo_append in
            let ys = List.append xs xs in
            let zs = ys in

            let xs' = I.of_list xs in
            let ys' = append xs' xs' in
            let zs' = I.to_list ys' in

            assert_list_equal ~msg:"initial" zs zs';

            let xs = x::xs in
            let ys = List.append xs xs in
            let zs = ys in

            let ys' = try
                I.push x xs';
                I.refresh ();
                ys'
            with Adapton.Exceptions.NonSelfAdjustingValue ->
                let xs' = I.of_list xs in
                let ys' = append xs' xs' in
                ys'
            in
            let zs' = I.to_list ys' in

            assert_list_equal ~msg:"update" zs zs';
        end;

        "map" >:: QC.forall (QC.pair (QC.list QC.int) QC.int) begin fun ( xs, x ) ->
            let fn = succ in
            let map_fn, _ = I.memo_map (module I) fn in

            let ys = List.map fn xs in
            let zs = ys in

            let xs' = I.of_list xs in
            let ys' = map_fn xs' in
            let zs' = I.to_list ys' in

            assert_list_equal ~msg:"initial" zs zs';

            let xs = x::xs in
            let ys = List.map fn xs in
            let zs = ys in

            let ys' = try
                I.push x xs';
                I.refresh ();
                ys'
            with Adapton.Exceptions.NonSelfAdjustingValue ->
                let xs' = I.of_list xs in
                let ys' = map_fn xs' in
                ys'
            in
            let zs' = I.to_list ys' in

            assert_list_equal ~msg:"update" zs zs';
        end;

        "quicksort" >:: QC.forall (QC.pair (QC.list QC.int) (QC.list (QC.list (QC.triple QC.bool QC.int QC.int))))
                ~incl:
                    [ ( [ 8; 0; -7; 3; -2; 7; -1 ],
                        [ [ ( true, -3, -4 ); ( false, -8, 6 ); ( true, 5, 8 ); ( true, 0, 6 ); ( true, -5, -2 ) ];
                            [ ( false, -4, -9 ) ];
                            [ ( false, -1, -7 ); ( false, -7, 5 ); ( true, -2, -3 ); ( true, -5, -6 ); ( false, -8, -7 ) ];
                            [ ( false, -9, -2 ); ( true, -8, 6 ); ( true, -7, -4 ); ( false, -1, -6 ); ( true, -4, 0 ); ( true, 1, 6 ) ];
                            [ ( true, -9, -5 ); ( true, 5, 5 ); ( false, 1, 1 ); ( true, 2, -9 ); ( false, 8, -8 ); ( false, 6, -4 ); ( false, 5, 6 ); ( true, -6, -6 ) ];
                            [ ( false, -2, -3 ); ( true, 7, 4 ); ( false, -9, -5 ) ] ] ) ]
                begin fun ( xs, kss ) ->
            Gc.compact (); (* try to make GC effects consistent across tests *)
            let quicksort, _ = I.memo_quicksort compare in

            let n = List.length xs in
            let ys = List.sort compare xs in
            let zs = ys in

            let xs' = I.of_list xs in
            let ys' = quicksort xs' in
            let zs' = I.to_list ys' in

            assert_list_equal ~msg:"initial" zs zs';

            ignore begin List.fold_left begin fun ( xs, xs', n ) ks ->
                let xs, n' = List.fold_left begin fun ( xs, n ) ( i, k, x ) ->
                    if n = 0 then
                        ( [ x ], 1 )
                    else
                        let k = abs k mod n in
                        if i then
                            let rec insert k acc = function
                                | x::xs when k > 0 -> insert (pred k) (x::acc) xs
                                | xs -> List.rev_append acc (x::xs)
                            in
                            ( insert k [] xs, succ n )
                        else
                            let rec delete k acc = function
                                | x::xs when k > 0 -> delete (pred k) (x::acc) xs
                                | _::xs -> List.rev_append acc xs
                                | [] -> failwith "delete"
                            in
                            ( delete k [] xs, pred n )
                end ( xs, n ) ks in
                let ys = List.sort compare xs in
                let zs = ys in

                let xs', ys' = try
                    ignore begin List.fold_left begin fun n ( i, k, x ) ->
                        if n = 0 then begin
                            I.push x xs';
                            1
                        end else
                            let k = abs k mod n in
                            if i then begin
                                let rec insert k xs' = match I.force xs' with
                                    | `Cons ( x', xs' ) when k > 0 -> insert (pred k) xs'
                                    | _ -> I.push x xs'
                                in
                                insert k xs';
                                succ n
                            end else begin
                                let rec delete k xs' = match I.force xs' with
                                    | `Cons ( x', xs' ) when k > 0 -> delete (pred k) xs'
                                    | `Cons _ -> ignore (I.pop xs')
                                    | `Nil -> failwith "delete"
                                in
                                delete k xs';
                                pred n
                            end
                    end n ks end;
                    I.refresh ();
                    ( xs', ys' )
                with Adapton.Exceptions.NonSelfAdjustingValue ->
                    let xs' = I.of_list xs in
                    let ys' = quicksort xs' in
                    ( xs', ys' )
                in
                let zs' = I.to_list ys' in

                assert_list_equal ~msg:"update" zs zs';
                ( xs, xs', n' )
            end ( xs, xs', n ) kss end
        end;

        "filter map" >:: QC.forall (QC.triple (QC.list QC.int) QC.int QC.int) begin fun ( ws, w, p ) ->
            let pred = (<) p in
            let filter_pred, _ = I.memo_filter pred in
            let fn = succ in
            let map_fn, _ = I.memo_map (module I) fn in

            let xs = List.filter pred ws in
            let ys = List.map fn xs in
            let zs = ys in

            let ws' = I.of_list ws in
            let xs' = filter_pred ws' in
            let ys' = map_fn xs' in
            let zs' = I.to_list ys' in

            assert_list_equal ~msg:"initial" zs zs';

            let ws = w::ws in
            let xs = List.filter pred ws in
            let ys = List.map fn xs in
            let zs = ys in

            let ys' = try
                I.push w ws';
                I.refresh ();
                ys'
            with Adapton.Exceptions.NonSelfAdjustingValue ->
                let ws' = I.of_list ws in
                let xs' = filter_pred ws' in
                let ys' = map_fn xs' in
                ys'
            in
            let zs' = I.to_list ys' in

            assert_list_equal ~msg:"update" zs zs';
        end;

        "filter map append" >:: QC.forall (QC.triple (QC.list QC.int) QC.int QC.int) begin fun ( vs, v, p ) ->
            let pred = (<) p in
            let filter_pred, _ = I.memo_filter pred in
            let fn = succ in
            let map_fn, _ = I.memo_map (module I) fn in
            let append, _ = I.memo_append in

            let ws = List.filter pred vs in
            let xs = List.map fn ws in
            let ys = List.append xs xs in
            let zs = ys in

            let vs' = I.of_list vs in
            let ws' = filter_pred vs' in
            let xs' = map_fn ws' in
            let ys' = append xs' xs' in
            let zs' = I.to_list ys' in

            assert_list_equal ~msg:"initial" zs zs';

            let vs = v::vs in
            let ws = List.filter pred vs in
            let xs = List.map fn ws in
            let ys = List.append xs xs in
            let zs = ys in

            let ys' = try
                I.push v vs';
                I.refresh ();
                ys'
            with Adapton.Exceptions.NonSelfAdjustingValue ->
                let vs' = I.of_list vs in
                let ws' = filter_pred vs' in
                let xs' = map_fn ws' in
                let ys' = append xs' xs' in
                ys'
            in
            let zs' = I.to_list ys' in

            assert_list_equal ~msg:"update" zs zs';
        end;

        "memo" >:: QC.forall (QC.pair (QC.list QC.int) QC.int) ~where:(fun ( xs, _ ) -> xs <> []) begin fun ( xs, x ) ->
            try
                Gc.compact (); (* try to avoid GC messing up with memoization *)
                let update_count = ref 0 in
                let fn s = incr update_count; succ s in
                let map_fn, _ = I.memo_map (module I) fn in
                let n = List.length xs in
                let xs = I.of_list xs in
                I.refresh (); (* to detect if non-self-adjusting *)
                let ys = map_fn xs in
                assert_int_equal ~msg:"initial" 0 !update_count;
                I.refresh ();
                ignore (I.to_list ys);
                assert_int_equal ~msg:"force" n !update_count;
                I.push x xs;
                ignore (I.to_list ys);
                assert_int_equal ~msg:"push and force" (n + 2) !update_count; (* the first and second elements *)
                ignore (I.pop xs);
                ignore (I.to_list ys);
                assert_int_equal ~msg:"pop and force" (n + 3) !update_count; (* the first element *)
            with Adapton.Exceptions.NonSelfAdjustingValue ->
                skip "not self-adjusting"
        end;

        "gc" >:: QC.forall (QC.list QC.int) begin fun xs ->
            try
                Gc.compact (); (* try to avoid GC messing up with GC counts *)
                let gc_count = ref 0 in
                let rec finalise xs =
                    Gc.finalise (fun _ -> incr gc_count) xs;
                    match I.force xs with `Cons (_, xs) -> finalise xs | `Nil -> ()
                in
                let map_succ, _ = I.memo_map (module I) succ in
                let n = List.length xs in
                let xs = I.of_list xs in
                let ys = map_succ xs in
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
                assert_int_equal ~msg:"compact ys" n !gc_count;
            with Adapton.Exceptions.NonSelfAdjustingValue ->
                skip "not self-adjusting"
        end;
    ]


let make_testsuite ( name, salist ) =
    name >::: [
        make_regression_testsuite salist
    ]


let testsuite = "TestSAList" >::: List.map make_testsuite Adapton.All.salist_list
