#!/usr/bin/env python

import json, os, psutil, random, subprocess, sys, time, threading, traceback
from collections import defaultdict, OrderedDict
from itertools import chain, imap, izip, product, cycle

runbenchmarkadapton_native = "%s%s%s" % ( os.path.splitext(__file__)[0], os.path.extsep, "native" )


def driver(( module, task, size, take, edit, monotonic, seed )):
    driver_start_time = time.time()
    rng = random.Random(seed)
    results = OrderedDict((
        ( "module", module ), ( "task", task ), ( "size", size ), ( "take", take ), ( "edit", edit ), ( "monotonic", monotonic ), ( "seed", seed ) ))
    try:
        cmd = [ runbenchmarkadapton_native, "-m", "SAList (%s)" % ( module, ), "-t", str(task), "-I", str(size), "-T", str(take), "-E", str(edit) ]
        if monotonic:
            cmd.append("-M")
        cmd.extend(( "-S", str(seed) ))
        native = subprocess.Popen(cmd, stdout=subprocess.PIPE, env={ "BENCHMARK_SALIST_ENV": " " * rng.randrange(4096) })
        results.update(json.load(native.stdout, object_pairs_hook=OrderedDict))
        return results

    except Exception, KeyboardInterrupt:
        error = traceback.format_exc()
        results["error"] = error
        print>>sys.stderr, error
        print>>sys.stderr, "%32s %24s %4d %10d %20d ... error (%9.2fs)" % ( module, task, take, size, seed, time.time() - driver_start_time )
        return results
    finally:
        try:
            native.kill()
        except:
            pass


def physical_cpu_count():
    if sys.platform.startswith("darwin"):
        cpu_count = int(subprocess.check_output([ "sysctl", "-n", "hw.physicalcpu" ]))
    elif sys.platform.startswith("linux"):
        import re
        re_cpuinfo = re.compile(r"siblings\s+: ([0-9]+).*?cpu cores\s+: ([0-9]+)", re.MULTILINE | re.DOTALL)
        cpu_count = int(sum(map(lambda m: float(m.group(2)) / float(m.group(1)), re_cpuinfo.finditer(open("/proc/cpuinfo").read()))))
    else:
        print>>sys.stderr, "Warning: physical_cpu_count() not implemented for %s; using %d" % ( sys.platform, multiprocessing.cpu_count() )
    global physical_cpu_count
    physical_cpu_count = lambda: cpu_count
    return cpu_count


if __name__ == "__main__":
    import argparse, errno, gzip, multiprocessing, pprint, re, urllib

    salist_re = re.compile(r"SAList \(([^)]+)\)")
    config = json.loads(subprocess.check_output([ runbenchmarkadapton_native, "-c" ]))
    config["modules"] = map(lambda m: salist_re.sub(r"\1", str(m)), config["modules"])
    config["baselines"] = [ config["modules"][-1] ]
    config["takes"] = { str(m["name"]) : str(m["take"]) for m in config["tasks"] }
    config["tasks"] = config["takes"].keys()

    parser = argparse.ArgumentParser()
    group = parser.add_mutually_exclusive_group()
    group.add_argument("-B", "--benchmark", metavar="DIRECTORY",
        help="run benchmark and store results in %(metavar)s (default: \"%(const)s\")", nargs="?", const="Results/SAList")
    group.add_argument("-L", "--label", metavar="LABEL",
        help="optionally append %(metavar)s to result directory")
    group.add_argument("-R", "--resummarize", metavar="DIRECTORY",
        help="resummarize benchmark data in %(metavar)s(s) (default: \"Results/SAList/latest\")", nargs="*")
    parser.add_argument("-P", "--processes", metavar="N", help="run %(metavar)s benchmarks in parallel", default=physical_cpu_count(), type=int)
    parser.add_argument("-m", "--modules", metavar="MODULE",
        help="apply benchmark to %(metavar)s(s) (default: \"%(default)s\")", nargs="+", default=config["modules"], choices=config["modules"])
    parser.add_argument("-b", "--baselines", metavar="BASELINE",
        help="compare modules against %(metavar)s(s) (default: \"%(default)s\")", nargs="+", default=config["baselines"], choices=config["modules"])
    parser.add_argument("-t", "--tasks", metavar="TASK",
        help="apply benchmark to %(metavar)s(s) (default: \"%(default)s\")", nargs="+", default=config["tasks"], choices=config["tasks"])
    parser.add_argument("-I", "--input-sizes", metavar="SIZE",
        help="run benchmarks with input list size (default: 100000 10000 1000 100 50000 5000 500 50 200000 20000 2000 200 20)",
        nargs="+", default=( 100000, 10000, 1000, 100, 50000, 5000, 500, 50, 20000, 2000, 200, 20 ), type=int)
    parser.add_argument("-T", "--take-counts", metavar="TAKE", help="take only the first %(metavar)s elements of each output list (default: 1)",
        nargs="+", default=( 1, ), type=int)
    parser.add_argument("-E", "--edit-count", metavar="COUNT", help="average self-adjusting benchmarks over %(metavar)s list edits ",
        default=250, type=int)
    parser.add_argument("-M", "--monotonic", help="make monotonic list edits ", action="store_true")
    parser.add_argument("-S", "--random-seeds", metavar="SEED", help="run benchmark for seeds (default: 5 random seeds)",
        nargs="+", default=random.sample(xrange(sys.maxint >> 1), 5), type=int)
    args = parser.parse_args()
    if len(args.input_sizes) > 1 and len(args.take_counts) > 1:
        parser.error("either -I/--input-sizes or -T/--take-counts must be given only one value")
    elif len(args.input_sizes) == 1 and len(args.take_counts) == 1:
        parser.error("-I/--input-sizes and -T/--take-counts must not both be given only one value")
    if len(args.take_counts) > 1 or args.take_counts[0] != 1:
        for task in args.tasks:
            if config["takes"][task] == "one":
                parser.error("-t/--tasks \"%s\" only supports -T/--take-counts 1" % ( task, ))
    if args.benchmark is None and args.resummarize is None:
        args.benchmark = "Results/SAList"
    elif args.resummarize == []:
        args.resummarize = [ "Results/SAList/latest" ]
    for baseline in args.baselines:
        if baseline not in args.modules:
            args.modules.append(baseline)

    if args.resummarize is not None:
        folder = sorted(set(args.resummarize))[-1]
        folders = args.resummarize
        results_dir = os.path.basename(folder)
    else:
        results_dir = time.strftime("%Y-%m-%d-%H-%M-%S")
        if args.monotonic:
            results_dir += " monotonic"
        if args.label:
            results_dir += " " + args.label.strip()
        folder = os.path.join(args.benchmark, results_dir)
        os.makedirs(folder)
        latest = os.path.join(args.benchmark, "latest")
        try:
            os.unlink(latest)
        except OSError:
            pass
        try:
            os.symlink(results_dir, latest)
        except OSError:
            print>>sys.stderr, traceback.format_exc()
            print>>sys.stderr, "warning: cannot create latest symlink to benchmark"
        folders = [ folder ]

        print>>sys.stderr, "Using random seeds %s" % ( args.random_seeds, )

        pool = multiprocessing.Pool(processes=args.processes, maxtasksperchild=1)

        @apply
        class heartbeat(object):
            def __init__(self):
                self.flags = []
            def __enter__(self):
                flag = threading.Event()
                thread = threading.Thread(target=self.run, args=( flag, ))
                thread.daemon = True
                self.flags.append(flag)
                thread.start()
            def __exit__(self, exc_type, exc_val, exc_tb):
                self.flags.pop().set()
            def run(self, flag):
                while not flag.is_set():
                    load = os.getloadavg()
                    free = psutil.virtual_memory().available / 1024 / 1024
                    print>>sys.stderr, "Load: %5.2f %5.2f %5.2f  Mem: %6dM free" % ( load[0], load[1], load[2], free )
                    flag.wait(3)

        with heartbeat:
            for task in args.tasks:
                results = []
                try:
                    # don't use pool.apply_async, it's triggers http://bugs.python.org/issue10332
                    for result in pool.imap_unordered(driver, ( ( module, task, size, take, args.edit_count, args.monotonic, seed )
                            for take in args.take_counts
                            for size in args.input_sizes
                            for seed in args.random_seeds
                            for module in random.sample(args.modules, len(args.modules)) )):
                        # don't use extend, so that if an exception or interrupt occurs, we still have some results
                        results.append(result)
                except Exception:
                    traceback.print_exc()
                except KeyboardInterrupt:
                    pool.terminate()
                    pool.join()
                    sys.exit()
                finally:
                    if len(args.input_sizes) > 1:
                        assert len(args.take_counts) == 1
                        results = OrderedDict((
                            ( "label", "take count = %d" % args.take_counts[0] ),
                            ( "x-axis", "size" ),
                            ( "x-label", "input size" ),
                            ( "data", results )
                        ))
                    else:
                        assert len(args.input_sizes) == 1 and len(args.take_counts) > 1
                        results = OrderedDict((
                            ( "label", "input size = %d" % args.input_sizes[0] ),
                            ( "x-axis", "take" ),
                            ( "x-label", "take count" ),
                            ( "data", results )
                        ))
                    with gzip.open(os.path.join(folder, "%s-%04d.json.gz" % ( task, len(results) )), "w") as jsonfile:
                        json.dump(results, jsonfile, indent=4, separators=( ",", ":" ))

    print>>sys.stderr, "Generating summary in %s ..." % ( folder, )
    files = sorted(set(chain.from_iterable( ( file for file in os.listdir(path) if file.endswith(".json.gz") ) for path in folders )))


    import math
    from decimal import Decimal
    from matplotlib.backends.backend_agg import FigureCanvasAgg as FigureCanvas
    from matplotlib.figure import Figure
    from matplotlib.ticker import Formatter
    from scipy import stats

    class Tee(object):
        def __init__(self, stream, name, *args, **kwargs):
            self.stream = stream
            self.file = open(name, *args, **kwargs)
        def __enter__(self):
            self.file.__enter__()
            return self
        def __exit__(self, exc_type, exc_val, exc_tb):
            return self.file.__exit__(exc_type, exc_val, exc_tb)
        def write(self, s):
            self.stream.write(s)
            self.file.write(s)

    class EngFormatter(Formatter):
        def __init__(self, places=3):
            self.places = places

        def __call__(self, num, pos=None):
            num = Decimal(str(num))

            if num != 0:
                exp = Decimal(math.floor(abs(num).log10() / 3) * 3)
            else:
                exp = Decimal(0)

            mantissa = num / ( 10 ** exp )
            result = "%.*g" % ( self.places, mantissa )
            if exp != 0:
                result += "e%+d" % ( exp, )
            return result

    styles = defaultdict(( { "color": color, "linestyle": linestyle, "marker": marker, "markersize": markersize }
        for color, ( linestyle, ( marker, markersize ) ) in izip(
            cycle(( "black", "darkblue", "darkred", "darkgreen", "darkcyan", "dimgray" )),
            cycle(product(( "-", "--", "-." ), ( ( ".", 8 ), ( "^", 6 ), ( "s", 4 ), ( "*", 7 ), ( "D", 4 ) ))) )).next)

    scalings = {
        "time": ( ( 1.01, ( "from-scratch", "propagate" ) ), (  1.01, ( "propagate", ) ) ),
        "heap": ( ( 1.01, ( "from-scratch", "propagate" ) ), ),
        "stack": ( ( 1.01, ( "from-scratch", "propagate" ) ), ),
        "max-heap": ( ( 1.01, ( "from-scratch", "propagate" ) ), ),
        "max-stack": ( ( 1.01, ( "from-scratch", "propagate" ) ), ),
    }


    with Tee(sys.stderr, os.path.join(folder, "summary.txt"), "w") as txtfile:
        with open(os.path.join(folder, "summary.html"), "w") as htmlfile:
            print>>htmlfile, "<!doctype html>"
            print>>htmlfile, "<meta charset=utf-8>"
            print>>htmlfile, "<head>"
            print>>htmlfile, "<title>%s</title>" % ( results_dir, )
            print>>htmlfile, "<style>figure.inline-figure { display: inline-block; margin: 0; }</style>"
            print>>htmlfile, "</head>"

            for file in files:
                print>>txtfile, "    Summarizing %s ..." % ( file, )
                label = file[:-8]
                summary = os.path.join(folder, label)
                print>>htmlfile, "<h1>%s</h1>" % ( label, )

                try:
                    os.makedirs(summary)
                except OSError as e:
                    if e.errno != errno.EEXIST:
                        raise

                results = None
                for path in folders:
                    filepath = os.path.join(path, file)
                    print>>txtfile, "        Loading %s ..." % ( filepath, ),
                    try:
                        more_results = json.load(gzip.open(filepath), object_pairs_hook=OrderedDict)
                        if not results:
                            results = more_results
                        else:
                            if more_results["label"] != results["label"]:
                                raise ValueError("inconsistent label in results:\nexpected: %s\ngot: %s" % ( results["label"], more_results["label"] ))
                            if more_results["x-axis"] != results["x-axis"]:
                                raise ValueError("inconsistent x-axis in results:\nexpected: %s\ngot: %s" % ( results["x-axis"], more_results["x-axis"] ))
                            if more_results["x-label"] != results["x-label"]:
                                raise ValueError("inconsistent x-label in results:\nexpected: %s\ngot: %s" % ( results["x-label"], more_results["x-label"] ))
                            results.extend(json.load(gzip.open(filepath), object_pairs_hook=OrderedDict))
                    except IOError as e:
                        if e.errno != errno.ENOENT:
                            raise
                        print>>txtfile, " not found"
                    except Exception:
                        traceback.print_exc()
                    else:
                        print>>txtfile, " done"

                table = OrderedDict( ( key, {} ) for key in ( "time", "heap", "stack", "max-heap", "max-stack" ) )
                units = {}
                editables = set()
                for record in results["data"]:
                    try:
                        for key in table.iterkeys():
                            table[key].setdefault("from-scratch", {}).setdefault(record["module"], {}) \
                                .setdefault(record[results["x-axis"]], []).append(record["setup"][key])
                        if units and units != record["units"]:
                            raise ValueError("inconsistent units in results:\nexpected: %s\ngot: %s" % ( pprint.pformat(units), pprint.pformat(record["units"]) ))
                        units.update(record["units"])
                    except Exception:
                        traceback.print_exc()
                        if "error" in record:
                            pprint.pprint(dict(record))
                    else:
                        if "edits" in record:
                            editables.add(record["module"])
                            try:
                                for key in table.iterkeys():
                                    if key.startswith("max-"):
                                        table[key].setdefault("propagate", {}).setdefault(record["module"], {}) \
                                            .setdefault(record[results["x-axis"]], []).append(record["edits"][key])
                                    else:
                                        table[key].setdefault("propagate", {}).setdefault(record["module"], {}) \
                                            .setdefault(record[results["x-axis"]], []).append(record["edits"]["update-" + key] + record["edits"]["take-" + key])
                                        table[key].setdefault("update", {}).setdefault(record["module"], {}) \
                                            .setdefault(record[results["x-axis"]], []).append(record["edits"]["update-" + key])
                            except Exception:
                                traceback.print_exc()
                                if "error" in record:
                                    pprint.pprint(dict(record))

                xmax = {}
                ymax = {}
                for measurement, measurement_table in table.iteritems():
                    xmax[measurement] = {}
                    ymax[measurement] = {}
                    for timing, module_table in measurement_table.iteritems():
                        for module, xvalues in module_table.iteritems():
                            for xvalue, yvalues in xvalues.iteritems():
                                avg = stats.tmean(yvalues)
                                xvalues[xvalue] = avg
                                xmax[measurement][timing] = max(xmax[measurement].get(timing, 0), xvalue)
                                ymax[measurement][timing] = max(ymax[measurement].get(timing, 0), avg)
                            module_table[module] = OrderedDict(sorted(xvalues.iteritems()))
                        measurement_table[timing] = OrderedDict(
                            sorted(module_table.iteritems(), key=lambda ( module, xvalues ): max(xvalues.itervalues()), reverse=True))

                for measurement, measurement_table in table.iteritems():
                    for yadjust, timings in scalings[measurement]:
                        print>>txtfile, "        Plotting %s (%.2fx of %s)" % ( measurement, yadjust, timings )
                        pdffilename = "%s-%s-%s-%s.pdf" % ( label, measurement, yadjust, "-".join(timings) )
                        with open(os.path.join(summary, pdffilename), "w") as pdffile:
                            fig = FigureCanvas(Figure(figsize=( 3.5, 3 ))).figure
                            ax = fig.add_subplot(1, 1, 1,
                                xlim=( 0, 1.01 * max( xmax[measurement][timing] for timing in timings ) ),
                                ylim=( 0, yadjust * max( ymax[measurement][timing] for timing in timings ) ))

                            ax.set_title(results["label"], fontsize=8)
                            ax.set_xlabel(results["x-label"], fontsize=8)
                            ax.set_ylabel("%s (%s)" % ( measurement, units[measurement] ), fontsize=8)
                            for axis in ( ax.get_xaxis(), ax.get_yaxis() ):
                                axis.set_major_formatter(EngFormatter())
                                axis.set_ticks_position("none")
                            if hasattr(ax, "tick_params"):
                                ax.tick_params(labelsize=7)
                            for side in ( "left", "bottom" ):
                                ax.spines[side].set_color("silver")
                                ax.spines[side].set_linestyle("dotted")
                                ax.spines[side].set_linewidth(0.5)
                            for side in ( "right", "top" ):
                                ax.spines[side].set_visible(False)
                            ax.grid(linewidth=0.5, linestyle=":", color="silver")

                            for timing in ( "from-scratch", "propagate" ):
                                module_table = measurement_table[timing]
                                for module, xvalues in module_table.iteritems():
                                    xvalues, yvalues = zip(*xvalues.iteritems())
                                    print>>txtfile, "            %50s ... %s" \
                                        % ( "%s (%s)" % ( module, timing ), " ".join( format(yvalue, "9.3g") for yvalue in yvalues ) )
                                    ax.plot(xvalues, yvalues, clip_on=False, label="%s (%s)" % ( module, timing ), markeredgecolor="none",
                                        **styles[module, timing])

                            try:
                                ax.legend(loc="best", prop={ "size": 8 }, frameon=False, fancybox=False)
                            except TypeError:
                                ax.legend(loc="best", prop={ "size": 8 }, fancybox=False)

                            if hasattr(fig, "tight_layout"):
                                fig.tight_layout(pad=0.5)

                            fig.savefig(pdffile, format="pdf")
                            print>>htmlfile, "<figure class=inline-figure><img src=%s></figure>" \
                                % ( os.path.join(label, urllib.pathname2url(pdffilename)), )


                for baseline in args.baselines:
                    pdffilename = "%s-%s-overhead.pdf" % ( label, baseline, )
                    with open(os.path.join(summary, pdffilename), "w") as pdffile:
                        fig = FigureCanvas(Figure(figsize=( 3.5, 3 ))).figure
                        ax = fig.add_subplot(1, 1, 1,
                            xlim=( 0, 1.01 * xmax["time"]["propagate"] ))
                        ax.set_title(results["label"], fontsize=8)
                        ax.set_xlabel(results["x-label"], fontsize=8)
                        ax.set_ylabel("time overhead\nX (from-scratch) / %s (from-scratch)" % ( baseline, ), fontsize=8, multialignment="center")
                        for axis in ( ax.get_xaxis(), ax.get_yaxis() ):
                            axis.set_major_formatter(EngFormatter())
                            axis.set_ticks_position("none")
                        if hasattr(ax, "tick_params"):
                            ax.tick_params(labelsize=7)
                        for side in ( "left", "bottom" ):
                            ax.spines[side].set_color("silver")
                            ax.spines[side].set_linestyle("dotted")
                            ax.spines[side].set_linewidth(0.5)
                        for side in ( "right", "top" ):
                            ax.spines[side].set_visible(False)
                        ax.grid(linewidth=0.5, linestyle=":", color="silver")

                        print>>txtfile, "        Plotting overhead using baseline %s ..." % ( baseline, )
                        for module in editables:
                            xvalues, overheads = zip(*( ( xvalue, yvalue / table["time"]["from-scratch"][baseline][xvalue] ) \
                                for xvalue, yvalue in table["time"]["from-scratch"][module].iteritems() ))
                            print>>txtfile, "            %32s ... %s" % ( module, " ".join( format(overhead, "9.3g") for overhead in overheads ) )
                            ax.plot(xvalues, overheads, clip_on=False, label=module, markeredgecolor="none", **styles[module, "from-scratch"])

                        try:
                            ax.legend(loc="best", prop={ "size": 8 }, frameon=False, fancybox=False)
                        except TypeError:
                            ax.legend(loc="best", prop={ "size": 8 }, fancybox=False)

                        if hasattr(fig, "tight_layout"):
                            fig.tight_layout(pad=0.5)

                        fig.savefig(pdffile, format="pdf")
                        print>>htmlfile, "<figure class=inline-figure><img src=%s></figure>" \
                            % ( os.path.join(label, urllib.pathname2url(pdffilename)), )


                for baseline in args.baselines:
                    pdffilename = "%s-%s-speedup.pdf" % ( label, baseline, )
                    with open(os.path.join(summary, pdffilename), "w") as pdffile:
                        fig = FigureCanvas(Figure(figsize=( 3.5, 3 ))).figure
                        ax = fig.add_subplot(1, 1, 1,
                            xlim=( 0, 1.01 * xmax["time"]["propagate"] ))
                        ax.set_title(results["label"], fontsize=8)
                        ax.set_xlabel(results["x-label"], fontsize=8)
                        ax.set_ylabel("time speed-up\n%s (from-scratch) / X (propagate)" % ( baseline, ), fontsize=8, multialignment="center")
                        for axis in ( ax.get_xaxis(), ax.get_yaxis() ):
                            axis.set_major_formatter(EngFormatter())
                            axis.set_ticks_position("none")
                        if hasattr(ax, "tick_params"):
                            ax.tick_params(labelsize=7)
                        for side in ( "left", "bottom" ):
                            ax.spines[side].set_color("silver")
                            ax.spines[side].set_linestyle("dotted")
                            ax.spines[side].set_linewidth(0.5)
                        for side in ( "right", "top" ):
                            ax.spines[side].set_visible(False)
                        ax.grid(linewidth=0.5, linestyle=":", color="silver")

                        print>>txtfile, "        Plotting speed-up using baseline %s ..." % ( baseline, )
                        for module in editables:
                            xvalues, speedups = zip(*( ( xvalue, table["time"]["from-scratch"][baseline][xvalue] / yvalue ) \
                                for xvalue, yvalue in table["time"]["propagate"][module].iteritems() ))
                            print>>txtfile, "            %32s ... %s" % ( module, " ".join( format(speedup, "9.3g") for speedup in speedups ) )
                            ax.plot(xvalues, speedups, clip_on=False, label=module, markeredgecolor="none", **styles[module, "propagate"])

                        try:
                            ax.legend(loc="best", prop={ "size": 8 }, frameon=False, fancybox=False)
                        except TypeError:
                            ax.legend(loc="best", prop={ "size": 8 }, fancybox=False)

                        if hasattr(fig, "tight_layout"):
                            fig.tight_layout(pad=0.5)

                        fig.savefig(pdffile, format="pdf")
                        print>>htmlfile, "<figure class=inline-figure><img src=%s></figure>" \
                            % ( os.path.join(label, urllib.pathname2url(pdffilename)), )


                for module in editables:
                    print>>txtfile, "        Plotting %s details ..." % ( module, )
                    pdffilename = "%s-%s-details.pdf" % ( label, module )
                    with open(os.path.join(summary, pdffilename), "w") as pdffile:
                        fig = FigureCanvas(Figure(figsize=( 3.5, 3 ))).figure
                        ax = fig.add_subplot(1, 1, 1,
                            xlim=( 0, 1.01 * xmax["time"]["propagate"] ))
                        ax.set_title("%s details; %s" % ( module, results["label"] ), fontsize=8)
                        ax.set_xlabel(results["x-label"], fontsize=8)
                        ax.set_ylabel("time (%s)" % ( units["time"], ), fontsize=8)
                        for axis in ( ax.get_xaxis(), ax.get_yaxis() ):
                            axis.set_major_formatter(EngFormatter())
                            axis.set_ticks_position("none")
                        if hasattr(ax, "tick_params"):
                            ax.tick_params(labelsize=7)
                        for side in ( "left", "bottom" ):
                            ax.spines[side].set_color("silver")
                            ax.spines[side].set_linestyle("dotted")
                            ax.spines[side].set_linewidth(0.5)
                        for side in ( "right", "top" ):
                            ax.spines[side].set_visible(False)
                        ax.grid(linewidth=0.5, linestyle=":", color="silver")

                        for timing in ( "propagate", "update" ):
                            xvalues, yvalues = zip(*table["time"][timing][module].iteritems())
                            print>>txtfile, "            %24s ... %s" \
                                % ( timing, " ".join( format(yvalue, "9.3g") for yvalue in yvalues ) )
                            ax.plot(xvalues, yvalues, clip_on=False, label="%s" % ( timing, ), markeredgecolor="none", **styles[module, timing])

                        try:
                            ax.legend(loc="best", prop={ "size": 8 }, frameon=False, fancybox=False)
                        except TypeError:
                            ax.legend(loc="best", prop={ "size": 8 }, fancybox=False)

                        if hasattr(fig, "tight_layout"):
                            fig.tight_layout(pad=0.5)

                        fig.savefig(pdffile, format="pdf")
                        print>>htmlfile, "<figure class=inline-figure><img src=%s></figure>" \
                            % ( os.path.join(label, urllib.pathname2url(pdffilename)), )
