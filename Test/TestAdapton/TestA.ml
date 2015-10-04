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
        end;

        "memo cache gc" >:::
            let test (type a) (module P : AdaptonUtil.Signatures.AType.S with type t = a) (memo_p : int -> a) =
                let module W = Weak.Make (struct include P let hash = hash 0 end) in
                let n = I.const 10 in
                let w = W.create 0 in
                ignore @@ I.force @@ I.thunk (fun () -> for k = 1 to (I.force n) do ignore @@ P.force (W.merge w (memo_p k)) done; 0);
                assert_int_equal ~msg:"count before gc" (I.force n) (W.count w);
                if L.is_incremental then (I.update_const n 0; L.refresh ());
                Gc.full_major ();
                assert_int_equal ~msg:"count after gc" 0 (W.count w)
            in [
                "lazy" >:: begin fun () ->
                    let module P = L.Make (struct
                        type t = [ `S of t L.thunk | `Z ]
                        let hash seed = function
                            | `S x -> L.hash seed x
                            | `Z -> seed
                        let equal x x' = x == x' || match x, x' with
                            | `S x, `S x' -> L.equal x x'
                            | _, _ -> false
                    end) in
                    test (module P) (P.memo (module AdaptonUtil.Types.Int) (fun memo_p n -> if n = 0 then `Z else `S (memo_p (n - 1))))
                end;

                "eager" >:: begin fun () ->
                    let module P = L.Make (struct
                        type t = [ `S of t | `Z ]
                        let rec hash seed = function
                            | `S x -> hash seed x
                            | `Z -> seed
                        let rec equal x x' = x == x' || match x, x' with
                            | `S x, `S x' -> equal x x'
                            | _, _ -> false
                    end) in
                    test (module P) (P.memo (module AdaptonUtil.Types.Int) (fun memo_p n -> if n = 0 then `Z else `S (L.force (memo_p (n - 1)))))
                end
            ]
    ]

let make_testsuite ( name, atype ) =
    name >::: [
        make_regression_testsuite atype;
        make_correctness_testsuite atype;
    ]


let testsuite = "TestA" >::: List.map make_testsuite AdaptonZoo.All.a_list
