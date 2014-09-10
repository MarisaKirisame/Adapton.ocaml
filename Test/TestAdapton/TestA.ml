open TestUtil.MyOUnit
open Format

let assert_int_equal = assert_equal ~printer:pp_print_int

let make_regression_testsuite (module L : AdaptonUtil.Signatures.AType) =
    let module I = L.Make (AdaptonUtil.Types.Int) in

    "Regression" >::: [
        "update const to thunk" >:: begin fun () ->
            let x = I.const 1 in
            let y = I.thunk (fun () -> I.force x) in
            assert_int_equal ~msg:"initial" 1 (I.force y);

            if L.is_incremental then begin
                let w = I.const 1 in
                I.update_thunk x (fun () -> I.force w);
                I.refresh ();
                assert_int_equal ~msg:"update x to thunk" 1 (I.force x);

                I.update_const w 2;
                I.refresh ();
                assert_int_equal ~msg:"update w to const" 2 (I.force y);
            end
        end;

        "update const to const to thunk" >:: begin fun () ->
            let x = I.const 1 in
            let y = I.thunk (fun () -> I.force x) in
            assert_int_equal ~msg:"initial" 1 (I.force y);

            if L.is_incremental then begin
                I.update_const x 2;
                I.refresh ();
                assert_int_equal ~msg:"update x to const" 2 (I.force y);

                let w = I.const 2 in
                I.update_thunk x (fun () -> I.force w);
                I.refresh ();
                assert_int_equal ~msg:"update x to thunk" 2 (I.force x);

                I.update_const w 3;
                I.refresh ();
                assert_int_equal ~msg:"update w to const" 3 (I.force y);
            end
        end;
    ]

let make_correctness_testsuite (module L : AdaptonUtil.Signatures.AType) =
    let module I = L.Make (AdaptonUtil.Types.Int) in

    "Correctness" >::: [
        "memo cache" >:: QC.forall (QC.pair QC.int QC.int) begin fun ( width, height ) ->
            let width = 4 * (20 + abs width) in
            let height = 20 + abs height in
            let repeat = 50 in

            let offset = I.const 0 in
            let xs = Array.init width (fun k -> I.const k) in

            let memo_chain =
                let memo_chains = Array.map begin fun _ -> I.memo (module I) begin fun memo_chain x ->
                    let rec chain acc n = if n = 0 then acc else chain (I.thunk (fun () -> I.force acc + 1)) (n - 1) in
                    I.force (chain (I.thunk (fun () -> I.force x + I.force offset)) height)
                end end xs in
                fun k -> memo_chains.(k)
            in

            let check msg = assert_equal ~msg ~eq:(array_equal (fun y z -> I.force y + I.force offset + height = I.force z)) xs (Array.mapi memo_chain xs) in

            check "initial";

            if L.is_incremental && L.is_lazy then begin
                for k = 1 to repeat do
                    ignore (Gc.major_slice (width * k * k));
                    I.update_const offset k;
                    check "memo"
                done
            end
        end
    ]

let make_testsuite ( name, atype ) =
    name >::: [
        make_regression_testsuite atype;
        make_correctness_testsuite atype;
    ]


let testsuite = "TestA" >::: List.map make_testsuite AdaptonZoo.All.a_list
