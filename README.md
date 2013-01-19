Introduction
============

Adapton.ocaml is an OCaml library for self-adjusting computation.


Quick-start
===========

1. Compile Adapton.ocaml:

        % make

2. Run Adapton.ocaml unit tests:

        % make test

3. Start an OCaml REPL with Adapton.ocaml loaded:

        % make repl
                OCaml version 4.00.1

        # module Int = struct
            type t = int
            let hash x = x
            let equal = (==)
        end;;
        # module IntList = Adapton.Default.SAList.Make (Int);;
        # let xs = IntList.of_list [1;2;3];;
        # let filter_gt_1, _ = IntList.filter (fun x -> x > 1);;
        # let map_succ, _ = IntList.map (module Int) succ;;
        # let ys = filter_gt_1 xs;;
        # let zs = map_succ xs;;
        # IntList.to_list ys;;
