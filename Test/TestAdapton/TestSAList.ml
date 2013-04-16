open TestUtil.MyOUnit
open Format

let assert_int_equal = assert_equal ~printer:pp_print_int
let assert_list_equal = assert_equal ~printer:(list_printer pp_print_int ",")

let make_regression_testsuite name (module L : Adapton.Signatures.SAListType) =
    let module I = L.Make (Adapton.Types.Int) in

    let test_salist_op_with_test ?(count=25) ?incl op sa_op ~test =
        Gc.compact (); (* try to make GC effects consistent across tests *)
        QC.forall ~count ?incl (QC.pair (QC.list QC.int) (QC.list (QC.list (QC.triple QC.bool QC.int QC.int)))) begin fun ( xs, kss ) ->
            let n = List.length xs in
            let ys = op xs in

            let xs' = I.of_list xs in
            let ys' = sa_op xs' in

            test ~msg:"initial" ys ys';

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
                let ys = op xs in

                let xs', ys' = if L.is_self_adjusting then begin
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
                end else begin
                    let xs' = I.of_list xs in
                    let ys' = sa_op xs' in
                    ( xs', ys' )
                end in

                test ~msg:"update" ys ys';
                ( xs, xs', n' )
            end ( xs, xs', n ) kss end
        end ()
    in

    let test_salist_op = test_salist_op_with_test ~test:(fun ~msg expected actual -> assert_list_equal ~msg expected (I.to_list actual)) in

    "Correctness" >::: [
        "filter" >:: QC.forall QC.int begin fun p ->
            let pred = (<) p in
            let filter_pred, _ = I.memo_filter pred in
            test_salist_op (List.filter pred) filter_pred
        end;

        "append" >:: begin fun () ->
            let append, _ = I.memo_append in
            test_salist_op (fun xs -> List.append xs xs) (fun xs' -> append xs' xs')
        end;

        "map" >:: begin fun () ->
            let fn = succ in
            let map_fn, _ = I.memo_map (module I) fn in
            test_salist_op (List.map fn) map_fn
        end;

        "tfold" >:: begin fun () ->
            let fn = (+) in
            let tfold_fn, _ = I.memo_tfold fn in
            test_salist_op_with_test
                ~test:(fun ~msg expected actual -> assert_int_equal ~msg expected (I.SAData.force actual))
                (List.fold_left fn 0) (fun xs' -> tfold_fn (I.const (`Cons ( 0, xs' )))) (* prepend 0 to avoid empty lists *)
        end;

        "quicksort" >:: begin fun () ->
            let quicksort, _ = I.memo_quicksort compare in
            test_salist_op
                ~incl:
                    [ ( [ 8; 0; -7; 3; -2; 7; -1 ],
                        [ [ ( true, -3, -4 ); ( false, -8, 6 ); ( true, 5, 8 ); ( true, 0, 6 ); ( true, -5, -2 ) ];
                            [ ( false, -4, -9 ) ];
                            [ ( false, -1, -7 ); ( false, -7, 5 ); ( true, -2, -3 ); ( true, -5, -6 ); ( false, -8, -7 ) ];
                            [ ( false, -9, -2 ); ( true, -8, 6 ); ( true, -7, -4 ); ( false, -1, -6 ); ( true, -4, 0 ); ( true, 1, 6 ) ];
                            [ ( true, -9, -5 ); ( true, 5, 5 ); ( false, 1, 1 ); ( true, 2, -9 ); ( false, 8, -8 ); ( false, 6, -4 ); ( false, 5, 6 ); ( true, -6, -6 ) ];
                            [ ( false, -2, -3 ); ( true, 7, 4 ); ( false, -9, -5 ) ] ] ) ]
                (List.sort compare) quicksort
        end;

        "mergesort" >:: begin fun () ->
            let mergesort, _ = I.memo_mergesort compare in
            test_salist_op (List.sort compare) mergesort
        end;

        "filter map" >:: QC.forall QC.int begin fun p ->
            let pred = (<) p in
            let filter_pred, _ = I.memo_filter pred in
            let fn = succ in
            let map_fn, _ = I.memo_map (module I) fn in
            test_salist_op (fun xs -> List.map fn (List.filter pred xs)) (fun xs' -> map_fn (filter_pred xs'))
        end;

        "filter map append" >:: QC.forall QC.int begin fun p ->
            let pred = (<) p in
            let filter_pred, _ = I.memo_filter pred in
            let fn = succ in
            let map_fn, _ = I.memo_map (module I) fn in
            let append, _ = I.memo_append in
            test_salist_op (fun xs -> let ys = List.map fn (List.filter pred xs) in List.append ys ys) (fun xs' -> let ys' = map_fn (filter_pred xs') in append ys' ys')
        end;

        "lazy memo" >:: QC.forall (QC.pair (QC.list QC.int) QC.int) ~where:(fun ( xs, _ ) -> xs <> []) begin fun ( xs, x ) ->
            if not L.is_lazy then
                skip "not lazy";
            if not L.is_self_adjusting then
                skip "not self-adjusting";
            Gc.compact (); (* try to avoid GC messing up with memoization *)
            let update_count = ref 0 in
            let fn s = incr update_count; succ s in
            let map_fn, _ = I.memo_map (module I) fn in
            let n = List.length xs in
            let xs = I.of_list xs in
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
        end;

        "gc" >:: QC.forall (QC.list QC.int) begin fun xs ->
            if not L.is_self_adjusting then
                skip "not self-adjusting";
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
        end;
    ]


let make_testsuite ( name, salist ) =
    name >::: [
        make_regression_testsuite name salist
    ]


let testsuite = "TestSAList" >::: List.map make_testsuite Adapton.All.salist_list
