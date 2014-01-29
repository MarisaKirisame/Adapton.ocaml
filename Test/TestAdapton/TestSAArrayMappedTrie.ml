open TestUtil.MyOUnit
open Format

let assert_float_option_equal = assert_equal ~printer:(option_printer pp_print_float)

let make_correctness_testsuite (module A : AdaptonUtil.Signatures.SAArrayMappedTrieType) =
    let module F = A.Make (AdaptonUtil.Types.Float) in

    "Correctness" >::: [
        "add" >:: QC.forall (QC.triple QC.float (QC.list (QC.pair QC.int QC.float)) (QC.list QC.int)) begin fun ( fseed, xs, ks ) ->
            let h x = abs (Hashtbl.seeded_hash (Hashtbl.hash fseed) x) in
            let ys = Hashtbl.create 0 in
            let zs = List.fold_left (fun zs ( k, v ) -> let k = h k in Hashtbl.add ys k v; F.memo_add zs k v) F.empty xs in
            List.iter begin fun k ->
                let k = h k in
                assert_float_option_equal (try Some (Hashtbl.find ys k) with Not_found -> None) (F.get zs k);
            end ks
        end
    ]


let make_testsuite ( name, saamt ) =
    name >::: [
        make_correctness_testsuite saamt
    ]


let testsuite = "TestSAArrayMappedTrie" >::: List.map make_testsuite AdaptonZoo.All.saamt_list
