open TestUtil.MyOUnit

(* There's something wrong with ocamlbuild's dynamic dependency that causes it to miss some transitive dependencies
    from one mlpack to another mlpack. A workaround is to make sure all mlpacks are listed somewhere in this file:
    either in working code, or in the dummy module below. *)
module OcamlbuildDependencies = struct
    open Adapton
end

let _ =
    run_test_tt_main begin "TestAdapton" >::: [
        TestAdapton.TestSA.testsuite;
        TestAdapton.TestSAList.testsuite;
    ] end
