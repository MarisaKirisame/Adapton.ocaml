Introduction
============

Adapton.ocaml is an OCaml library for incremental computation.


Quick-start
===========

1. Compile Adapton.ocaml:

        % make

2. Run Adapton.ocaml unit tests:

        % make test

3. Start an OCaml REPL with Adapton.ocaml loaded:

        % make repl
                OCaml version 4.00.1

        # module IntList = Adapton.AList.Make (AdaptonUtil.Types.Int);;
        # let xs = IntList.of_list [1;2;3];;
        # let filter_gt_1 = IntList.memo_filter (fun x -> x > 1);;
        # let map_succ = IntList.memo_map (module IntList) succ;;
        # let ys = filter_gt_1 xs;;
        # let zs = map_succ ys;;
        # IntList.to_list zs;;
        - : IntList.data list = [3; 4]
        # IntList.insert 0 10 xs;;
        # IntList.to_list zs;;
        - : IntList.data list = [11; 3; 4]

4. Run Adapton.ocaml benchmarks:

        % make pldi2014-benchmarks

    Results will be located in Results/BenchmarkAdapton.


Requirements
============

To compile Adapton.ocaml:

* [OCaml](http://ocaml.org)
* [Findlib](http://projects.camlcity.org/projects/findlib.html)

To test Adapton.ocaml:

* [OUnit](http://ounit.forge.ocamlcore.org)

To benchmark Adapton.ocaml:

* [Python](http://www.python.org)
* [matplotlib](http://matplotlib.org)
* [scipy](http://www.scipy.org)
