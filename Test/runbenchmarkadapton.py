#!/usr/bin/env python

import json, os, psutil, random, subprocess, sys, time, threading, traceback
from collections import defaultdict, OrderedDict
from itertools import chain, imap, izip, product, cycle

runbenchmarkadapton_native = "%s%s%s" % ( os.path.splitext(__file__)[0], os.path.extsep, "native" )


def driver(( module, task, size, repeat, take, edit, monotonic, seed )):
    driver_start_time = time.time()
    rng = random.Random(seed)
    results = OrderedDict((
        ( "module", module ), ( "task", task ),
        ( "size", size ), ( "repeat", repeat), ( "take", take ), ( "edit", edit ), ( "monotonic", monotonic ),
        ( "seed", seed ) ))
    try:
        cmd = [ runbenchmarkadapton_native, "-m", "%s" % ( module, ), "-t", str(task), "-I", str(size), "-R", str(repeat), "-T", str(take), "-E", str(edit) ]
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

    config = json.loads(subprocess.check_output([ runbenchmarkadapton_native, "-c" ]))
    config["modules"] = map(str, config["modules"])
    config["baselines"] = [ config["modules"][-1] ]
    config["takes"] = { str(m["name"]) : str(m["take"]) for m in config["tasks"] }
    config["tasks"] = config["takes"].keys()
    config["output"] = "Results/SAList"

    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(dest="subparser")

    benchmark = subparsers.add_parser("benchmark", help="run benchmark")
    benchmark.add_argument("-O", "--output", metavar="DIRECTORY",
        help="run benchmark and store results in a subdirectory of %(metavar)s (default: \"%(const)s\")", default=config["output"])
    benchmark.add_argument("-L", "--label", metavar="LABEL", help="optionally append %(metavar)s to result directory")
    benchmark.add_argument("-P", "--processes", metavar="N", help="run %(metavar)s benchmarks in parallel", default=physical_cpu_count(), type=int)
    benchmark.add_argument("-m", "--modules", metavar="MODULE",
        help="apply benchmark to %(metavar)s(s) (default: \"%(default)s\")", nargs="+", default=config["modules"], choices=config["modules"])
    benchmark.add_argument("-b", "--baselines", metavar="BASELINE",
        help="compare modules against %(metavar)s(s) (default: \"%(default)s\")", nargs="+", default=config["baselines"], choices=config["modules"])
    benchmark.add_argument("-t", "--tasks", metavar="TASK",
        help="apply benchmark to %(metavar)s(s) (default: \"%(default)s\")", nargs="+", default=config["tasks"], choices=config["tasks"])
    benchmark.add_argument("-I", "--input-sizes", metavar="SIZE",
        help="run benchmarks with input size (default: 100000 10000 1000 100 50000 5000 500 50 200000 20000 2000 200 20)",
        nargs="+", default=( 100000, 10000, 1000, 100, 50000, 5000, 500, 50, 20000, 2000, 200, 20 ), type=int)
    benchmark.add_argument("-R", "--repeat-count", metavar="REPEAT",
        help="repeat the computation on the same input %(metavar)s times per cycle (default: 1)",
        default=1, type=int)
    benchmark.add_argument("-T", "--take-counts", metavar="TAKE", help="take only the first %(metavar)s elements of each output (default: 1)",
        nargs="+", default=( 1, ), type=int)
    benchmark.add_argument("-E", "--edit-count", metavar="COUNT", help="average self-adjusting benchmarks over %(metavar)s edits ",
        default=250, type=int)
    benchmark.add_argument("-M", "--monotonic", help="make monotonic edits ", action="store_true")
    benchmark.add_argument("-S", "--random-seeds", metavar="SEED", help="run benchmark for seeds (default: 5 random seeds)",
        nargs="+", default=random.sample(xrange(sys.maxint >> 1), 5), type=int)

    resummarize = subparsers.add_parser("resummarize", help="resummarize benchmark results")
    resummarize.add_argument("-I", "--inputs", metavar="DIRECTORY",
        help="resummarize benchmark results in %(metavar)s(s) (default: \"%(default)s\")", nargs="+", default=( os.path.join(config["output"], "latest"), ))
    resummarize.add_argument("-O", "--output", metavar="DIRECTORY",
        help="save benchmark summary in %(metavar)s (default: if only one directory is given for -I/--inputs, the same directory; "
            + "otherwise, a subdirectory in \"%s\")" % ( config["output"], ), nargs="?")
    resummarize.add_argument("-L", "--label", metavar="LABEL", help="optionally append %(metavar)s to summary directory")
    resummarize.add_argument("-b", "--baselines", metavar="BASELINE",
        help="compare modules against %(metavar)s(s) (default: \"%(default)s\")", nargs="+", default=config["baselines"], choices=config["modules"])

    args = parser.parse_args()

    if args.subparser == "resummarize":
        inputs = args.inputs
        if args.output is None:
            if len(inputs) == 1:
                output = inputs[0]
                if args.label:
                    output += " " + args.label.strip()
                    os.makedirs(output)
                if inputs[0] == os.path.join(config["output"], "latest") and os.path.islink(inputs[0]):
                    output_label = os.path.basename(os.readlink(inputs[0]))
                else:
                    output_label = os.path.basename(output)
            else:
                output_label = time.strftime("%Y-%m-%d-%H-%M-%S summary")
                if args.label:
                    output_label += " " + args.label.strip()
                output = os.path.join(config["output"], output_label)
                os.makedirs(output)
                latest = os.path.join(config["output"], "latest")
                try:
                    os.unlink(latest)
                except OSError:
                    pass
                try:
                    os.symlink(output_label, latest)
                except OSError:
                    print>>sys.stderr, traceback.format_exc()
                    print>>sys.stderr, "warning: cannot create latest symlink to summary"
        else:
            output = args.output
            if args.label:
                output += " " + args.label.strip()
            os.makedirs(output)
            output_label = os.path.basename(output)
    else:
        assert args.subparser == "benchmark"
        if args.processes < 1 or args.processes > physical_cpu_count():
            parser.error("-P/--processes must be between 1 and %d (the total number of physical processor cores)" % ( physical_cpu_count(), ))
        if len(set(args.input_sizes)) > 1 and len(set(args.take_counts)) > 1:
            parser.error("either -I/--input-sizes or -T/--take-counts must be given only one unique value")
        elif len(set(args.input_sizes)) == 1 and len(set(args.take_counts)) == 1:
            parser.error("-I/--input-sizes and -T/--take-counts must not both be given only one unique value each")
        if min(args.input_sizes) < 4:
            for task in args.tasks:
                if config["takes"][task] == "exptree":
                    parser.error("-t/--tasks \"%s\" only supports -I/--input-sizes n where n >= 4" % ( task, ))
        if len(set(args.take_counts)) > 1 or any(take != 1 for take in args.take_counts):
            for task in args.tasks:
                if config["takes"][task] == "one" or config["takes"][task] == "exptree" :
                    parser.error("-t/--tasks \"%s\" only supports -T/--take-counts 1" % ( task, ))
        if args.monotonic:
            for task in args.tasks:
                if config["takes"][task] == "flip":
                    parser.error("-t/--tasks \"%s\" does not support -M/--monotonic" % ( task, ))
        for baseline in args.baselines:
            if baseline not in args.modules:
                args.modules.append(baseline)

        output_label = time.strftime("%Y-%m-%d-%H-%M-%S")
        if args.monotonic:
            output_label += " monotonic"
        if args.label:
            output_label += " " + args.label.strip()
        output = os.path.join(args.output, output_label)
        os.makedirs(output)
        inputs = [ output ]
        latest = os.path.join(args.output, "latest")
        try:
            os.unlink(latest)
        except OSError:
            pass
        try:
            os.symlink(output_label, latest)
        except OSError:
            print>>sys.stderr, traceback.format_exc()
            print>>sys.stderr, "warning: cannot create latest symlink to benchmark"

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
                start = time.time()
                max_load = 0
                min_free = float("inf")
                while not flag.is_set():
                    elapsed = time.time() - start
                    load = os.getloadavg()
                    free = psutil.virtual_memory().free / 1024 / 1024
                    max_load = max(max_load, *load)
                    min_free = min(min_free, free)
                    print>>sys.stderr, "==== Elapsed: %5ds  ==== Load: %5.2f %5.2f %5.2f (max: %5.2f) ==== Mem: %6dM free (min: %6dM) ====" \
                        % ( elapsed, load[0], load[1], load[2], max_load, free, min_free )
                    flag.wait(3)

        with heartbeat:
            for task in args.tasks:
                results = []
                try:
                    # don't use pool.apply_async, it triggers http://bugs.python.org/issue10332
                    for result in pool.imap_unordered(driver, ( ( module, task, size, args.repeat_count, take, args.edit_count, args.monotonic, seed )
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
                    with gzip.open(os.path.join(output, "%s-%04d.json.gz" % ( task, len(results) )), "w") as jsonfile:
                        json.dump(results, jsonfile, indent=4, separators=( ",", ":" ))

    print>>sys.stderr, "Generating summary in %s ..." % ( output, )
    files = sorted(set(chain.from_iterable( ( file for file in os.listdir(path) if file.endswith(".json.gz") ) for path in inputs )))


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

    scalings = defaultdict(lambda: ( ( 1.01, ( "from-scratch", "propagate" ) ), ))
    scalings["time"] += ( (  1.01, ( "propagate", ) ), )
    scalings["evaluate"] += ( (  1.01, ( "propagate", ) ), )


    with open(os.path.join(output, "index.html"), "w") as htmlfile:
        print>>htmlfile, "<!doctype html>"
        print>>htmlfile, "<meta charset=utf-8>"
        print>>htmlfile, "<title>%s</title>" % ( output_label, )
        print>>htmlfile, "<style>figure.inline-figure { display: inline-block; margin: 0; }</style>"

        with Tee(sys.stderr, os.path.join(output, "summary.txt"), "w") as txtfile:
            for file in files:
                print>>txtfile, "    Summarizing %s ..." % ( file, )
                label = file[:-8]
                summary = os.path.join(output, label)
                print>>htmlfile, "<h1 id=\"%s\">%s</h1>" % ( label, label )

                try:
                    os.makedirs(summary)
                except OSError as e:
                    if e.errno != errno.EEXIST:
                        raise

                results = None
                for path in inputs:
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
                            results["data"].extend(more_results["data"])
                    except IOError as e:
                        if e.errno != errno.ENOENT:
                            raise
                        print>>txtfile, " not found"
                    except Exception:
                        traceback.print_exc()
                    else:
                        print>>txtfile, " done"

                table = OrderedDict( ( key, {} ) for key in ( "time", "heap", "stack", "update", "evaluate", "clean", "dirty", "max-heap", "max-stack" ) )
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
                                            .setdefault(record[results["x-axis"]], []).append(record["edits"]["update"][key] + record["edits"]["take"][key])
                                        table[key].setdefault("update", {}).setdefault(record["module"], {}) \
                                            .setdefault(record[results["x-axis"]], []).append(record["edits"]["update"][key])
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
                        if timings == ( "propagate", ) and not editables:
                            continue
                        print>>txtfile, "        Plotting %s (%.2fx of %s)" % ( measurement, yadjust, timings )
                        pdffilename = "%s-%s-%s-%s.pdf" % ( label, measurement, yadjust, "-".join(timings) )
                        with open(os.path.join(summary, pdffilename), "w") as pdffile:
                            fig = FigureCanvas(Figure(figsize=( 3.5, 3 ))).figure
                            ax = fig.add_subplot(1, 1, 1)
                            ax.set_title(results["label"], fontsize=8)
                            ax.set_xlabel(results["x-label"], fontsize=8)
                            if units[measurement]:
                                ax.set_ylabel("%s (%s)" % ( measurement, units[measurement] ), fontsize=8)
                            else:
                                ax.set_ylabel("%s" % ( measurement, ), fontsize=8)
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
                                if timing == "propagate" and not editables:
                                    continue
                                module_table = measurement_table[timing]
                                for module, xvalues in module_table.iteritems():
                                    xvalues, yvalues = zip(*xvalues.iteritems())
                                    print>>txtfile, "            %50s ... %s" \
                                        % ( "%s (%s)" % ( module, timing ), " ".join( format(yvalue, "9.3g") for yvalue in yvalues ) )
                                    ax.plot(xvalues, yvalues, clip_on=False, label="%s (%s)" % ( module, timing ), markeredgecolor="none",
                                        **styles[module, timing])
                            ax.set_xbound(lower=0)
                            ax.set_ybound(lower=0, upper=yadjust * max( ymax[measurement].get(timing, 0) for timing in timings ))

                            try:
                                ax.legend(loc="best", prop={ "size": 8 }, frameon=False, fancybox=False)
                            except TypeError:
                                ax.legend(loc="best", prop={ "size": 8 }, fancybox=False)

                            if hasattr(fig, "tight_layout"):
                                fig.tight_layout(pad=0.5)

                            fig.savefig(pdffile, format="pdf")
                            print>>htmlfile, "<figure class=inline-figure><img src=%s></figure>" \
                                % ( os.path.join(label, urllib.pathname2url(pdffilename)), )


                if editables:
                    for measurement in ( "time", "evaluate" ):
                        for baseline in args.baselines:
                            pdffilename = "%s-%s-%s-overhead.pdf" % ( label, baseline, measurement )
                            with open(os.path.join(summary, pdffilename), "w") as pdffile:
                                fig = FigureCanvas(Figure(figsize=( 3.5, 3 ))).figure
                                ax = fig.add_subplot(1, 1, 1)
                                ax.set_title(results["label"], fontsize=8)
                                ax.set_xlabel(results["x-label"], fontsize=8)
                                ax.set_ylabel("%s overhead\nX (from-scratch) / %s (from-scratch)" % ( measurement, baseline ), fontsize=8, multialignment="center")
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

                                print>>txtfile, "        Plotting %s overhead using baseline %s ..." % ( measurement, baseline )
                                for module in table[measurement]["from-scratch"].iterkeys():
                                    if module == baseline:
                                        continue
                                    xvalues, overheads = zip(*( ( xvalue, yvalue / table[measurement]["from-scratch"][baseline][xvalue] ) \
                                        for xvalue, yvalue in table[measurement]["from-scratch"][module].iteritems() ))
                                    print>>txtfile, "            %32s ... %s" % ( module, " ".join( format(overhead, "9.3g") for overhead in overheads ) )
                                    ax.plot(xvalues, overheads, clip_on=False, label=module, markeredgecolor="none", **styles[module, "from-scratch"])
                                ax.set_xbound(lower=0)
                                ax.set_ybound(lower=0)

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
                            pdffilename = "%s-%s-%s-speedup.pdf" % ( label, baseline, measurement )
                            with open(os.path.join(summary, pdffilename), "w") as pdffile:
                                fig = FigureCanvas(Figure(figsize=( 3.5, 3 ))).figure
                                ax = fig.add_subplot(1, 1, 1)
                                ax.set_title(results["label"], fontsize=8)
                                ax.set_xlabel(results["x-label"], fontsize=8)
                                ax.set_ylabel("%s speed-up\n%s (from-scratch) / X" % ( measurement, baseline ), fontsize=8, multialignment="center")
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

                                print>>txtfile, "        Plotting %s speed-up using baseline %s ..." % ( measurement, baseline )
                                for module in table[measurement]["from-scratch"].iterkeys():
                                    if module == baseline:
                                        continue
                                    if module in editables:
                                        timing = "propagate"
                                    else:
                                        timing = "from-scratch"
                                    xvalues, speedups = zip(*( ( xvalue, table[measurement]["from-scratch"][baseline][xvalue] / yvalue ) \
                                        for xvalue, yvalue in table[measurement][timing][module].iteritems() ))
                                    print>>txtfile, "            %50s ... %s" \
                                        % ( "%s (%s)" % ( module, timing ), " ".join( format(speedup, "9.3g") for speedup in speedups ) )
                                    ax.plot(xvalues, speedups, clip_on=False, label="%s (%s)" % ( module, timing ), markeredgecolor="none",
                                        **styles[module, timing])
                                ax.set_xbound(lower=0)
                                ax.set_ybound(lower=0)

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
                            ax = fig.add_subplot(1, 1, 1)
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
                            ax.set_xbound(lower=0)
                            ax.set_ybound(lower=0)

                            try:
                                ax.legend(loc="best", prop={ "size": 8 }, frameon=False, fancybox=False)
                            except TypeError:
                                ax.legend(loc="best", prop={ "size": 8 }, fancybox=False)

                            if hasattr(fig, "tight_layout"):
                                fig.tight_layout(pad=0.5)

                            fig.savefig(pdffile, format="pdf")
                            print>>htmlfile, "<figure class=inline-figure><img src=%s></figure>" \
                                % ( os.path.join(label, urllib.pathname2url(pdffilename)), )

        import cgi
        print>>htmlfile, "<h1 id=\"summary\">Summary</h1>"
        print>>htmlfile, "<pre>"
        print>>htmlfile, cgi.escape(open(os.path.join(output, "summary.txt")).read())
        print>>htmlfile, "</pre>"

        print>>htmlfile, "<style>div#header { position: fixed; top: 0; right: 0; padding: 0.5em; background: white }</style>"
        print>>htmlfile, "<div id=\"header\">"
        print>>htmlfile, "<a href=\"#summary\">Summary</a>"
        for label in ( file[:-8] for file in files ):
            print>>htmlfile, "<a href=\"#%s\">%s</a>" % ( label, label )
        print>>htmlfile, "</div>"
