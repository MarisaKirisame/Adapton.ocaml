open TestUtil.MyOUnit
open Format

module Int = struct
    type t = int
    let hash x = x
    let equal = (==)
end
let assert_list_equal = assert_equal ~printer:(list_printer pp_print_int ",")

let make_regression_testsuite (module L : Adapton.Signatures.SAListType) =
    let module I = L.Make (Int) in

    "]" >::: [
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

            I.push 4 xs';
            I.refresh ();
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

            I.push 4 xs';
            I.refresh ();
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

            I.push 4 xs';
            I.refresh ();
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

            I.push 4 ws';
            I.refresh ();
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

            I.push 4 vs';
            I.refresh ();
            let zs' = I.to_list ys' in

            assert_list_equal ~msg:"update" zs zs';
        end;
    ]


let make_testsuite ( name, salist ) =
    name >::: [
        make_regression_testsuite salist
    ]


let testsuite = "TestSAList" >::: List.map make_testsuite Adapton.All.salist_list
