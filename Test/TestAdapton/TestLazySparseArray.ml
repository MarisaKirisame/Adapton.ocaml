open TestUtil.MyOUnit
open Adapton
open Format

let assert_int_option_equal = assert_equal ~printer:(option_printer pp_print_int)


let testsuite = "TestLazySparseArray" >::: [
    "Correctness" >::: [
        "make" >:: QC.forall ~size:(fun n -> 1 + n mod LazySparseArray.size) (QC.pair (QC.array (QC.option QC.int)) (QC.list QC.int)) begin fun ( xs, ks ) ->
            let thunk k = let size = Array.length xs in if size == 0 then None else xs.(k mod size) in
            let ys = LazySparseArray.make thunk in
            List.iter begin fun k ->
                let k = abs k in
                if k < LazySparseArray.size then
                    assert_int_option_equal (thunk k) (LazySparseArray.get ys k)
                else
                    assert_raises (Invalid_argument "index out of bounds") (fun () -> LazySparseArray.get ys k)
            end ks
        end
    ]
]
