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


let make_testsuite ( name, atype ) =
    name >::: [
        make_regression_testsuite atype
    ]


let testsuite = "TestA" >::: List.map make_testsuite AdaptonZoo.All.a_list
