open TestUtil.MyOUnit
open Format


let make_correctness_testsuite configs =
    "Correctness" >::: List.map begin fun ( label, float_hash ) ->
        label >:: QC.forall (QC.pair QC.int (QC.list (QC.list QC.float))) begin fun ( seed, xss ) ->
            let module F = AdaptonInternal.WeakSet.Make (struct include AdaptonUtil.Types.Float let hash = float_hash seed end) in
            let ys = Hashtbl.create 0 in
            let zs = F.create 0 in
            List.iter begin fun xs ->
                List.iter begin fun x ->
                    Hashtbl.replace ys x ();
                    assert_equal ~printer:pp_print_float (F.merge zs x) x;
                end xs;
                let ws = F.fold (fun z ws -> z::ws) zs [] in
                assert_mem ~printer:pp_print_float (fun y ys -> Hashtbl.mem ys y) ws ys;
            end xss
        end
    end configs


let testsuite = "TestWeakSet" >::: [
    make_correctness_testsuite [
        ( "uniform", Hashtbl.seeded_hash );
        ( "limited", (fun seed x -> (Hashtbl.seeded_hash seed x) mod 2) );
        ( "skewed", (fun seed x -> let h = Hashtbl.seeded_hash seed x in if h land 1 == 1 then Hashtbl.seeded_hash seed seed else h) );
    ];
]
