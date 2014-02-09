Introduction
============

Adapton.ocaml is an OCaml library for incremental computation.


Quick-start
===========

1. Compile Adapton.ocaml:

        % make

2. Run Adapton.ocaml unit tests:

        % make test
        2014-02-07T14:05:38+00:00 hostname#00 I: Summary:
        ... lots of lines ...
        2014-02-07T14:05:38+00:00 hostname#00 I: Ran: 53 tests in: 33.42 seconds.
        2014-02-07T14:05:38+00:00 hostname#00 I: Cases: 53.
        2014-02-07T14:05:38+00:00 hostname#00 I: Tried: 53.
        2014-02-07T14:05:38+00:00 hostname#00 I: Errors: 0.
        2014-02-07T14:05:38+00:00 hostname#00 I: Failures: 0.
        2014-02-07T14:05:38+00:00 hostname#00 I: Skip: 5.
        2014-02-07T14:05:38+00:00 hostname#00 I: Todo: 0.
        2014-02-07T14:05:38+00:00 hostname#00 I: Timeout: 0.

    Make sure that there are no errors, failures or timeouts (a few skips are expected).

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

    * for systems with at least 8 cores and 16GB memory (runs up to 8 benchmarks in parallel)

            % make pldi2014-benchmarks

    * for smaller systems with at least 1GB memory (similar to above except 10% of the input sizes, not run in parallel)

            % make small-pldi2014-benchmarks

    * for the impatient (similar to above except 1% of the input sizes, not run in parallel)

            % make tiny-pldi2014-benchmarks

    Note the larger benchmark will take a good part of a day to complete, whereas the smaller benchmark will take a few
    hours, and the tiny benchmark takes a few minutes. Results will be written to
    `Results/BenchmarkAdapton/*pldi2014-benchmarks-*/{lazy,batch,swap,switch}/summary` for a HTML or text summary of the
    results, in particular, look under the _incremental_ columns:

                                    Adapton |                         EagerTotalOrder
                                incremental |                             incremental
                        speed-up | max-heap |                     speed-up | max-heap
        EagerNonInc | LazyNonInc |    bytes |     EagerNonInc | LazyNonInc |    bytes
        ------------------------------------- ... -----------------------------------
          63.9e+0   |   120e+0   | 175e+6   |        161e+0 * |   301e+0 * | 169e+6 *
          3.21e+0   |  2.85e+0   | 166e+6 * |        8.3e+0 * |  7.37e+0 * | 169e+6
           674e+0 * |   630e+0 * | 163e+6 * |        493e+0   |   461e+0   | 169e+6

    Cells marked `*` show the highest speed-up over *NonInc or smallest memory usage.


Requirements
============

To compile Adapton.ocaml:

* [OCaml](http://ocaml.org)
* [Findlib](http://projects.camlcity.org/projects/findlib.html)
* [Menhir](http://cristal.inria.fr/~fpottier/menhir)

To test Adapton.ocaml:

* [OUnit](http://ounit.forge.ocamlcore.org)

To benchmark Adapton.ocaml:

* [Python](http://www.python.org)
* [psutil](http://code.google.com/p/psutil)
* [matplotlib](http://matplotlib.org)
* [scipy](http://www.scipy.org)
