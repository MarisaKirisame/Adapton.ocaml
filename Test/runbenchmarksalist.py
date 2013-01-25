#!/usr/bin/env python

import json, os, random, subprocess, sys, time, traceback
from collections import defaultdict, OrderedDict
from itertools import chain, imap, izip, product, cycle

runbenchmarkadapton_native = "%s%s%s" % ( os.path.splitext(__file__)[0], os.path.extsep, "native" )


def driver(( module, task, size, take, edit, seed )):
    driver_start_time = time.time()
    rng = random.Random(seed)
    results = OrderedDict(( ( "module", module ), ( "task", task ), ( "size", size ), ( "take", take ), ( "edit", edit ), ( "seed", seed ) ))
    try:
        native = subprocess.Popen(
            [ runbenchmarkadapton_native, "-m", str(module), "-t", str(task), "-I", str(size), "-T", str(take), "-E", str(edit), "-S", str(seed) ],
            stdout=subprocess.PIPE,
            env={ "BENCHMARK_SALIST_ENV": " " * rng.randrange(4096) })
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
    import argparse, errno, gzip, multiprocessing, pprint, urllib

    config = json.loads(subprocess.check_output([ runbenchmarkadapton_native, "-c" ]))
    config["baselines"] = [ str(config["modules"][-1]) ]
    config["modules"] = map(str, config["modules"])
    config["tasks"] = map(str, config["tasks"])

    parser = argparse.ArgumentParser()
    group = parser.add_mutually_exclusive_group()
    group.add_argument("-B", "--benchmark", metavar="DIRECTORY",
        help="run benchmark and store results in %(metavar)s (default: \"%(const)s\")", nargs="?", const="Results/SAList")
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
    parser.add_argument("-T", "--take-count", metavar="TAKE", help="take only the first %(metavar)s elements of each output list default (1 10 5 2)",
        nargs="+", default=( 1, 10, 5, 2 ), type=int)
    parser.add_argument("-E", "--edit-count", metavar="COUNT", help="average self-adjusting benchmarks over %(metavar)s list edits ",
        default=250, type=int)
    parser.add_argument("-S", "--random-seeds", metavar="SEED", help="run benchmark for seeds (default: 5 random seeds)",
        nargs="+", default=random.sample(xrange(sys.maxint >> 1), 5), type=int)
    args = parser.parse_args()
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
    else:
        results_dir = time.strftime("%Y-%m-%d-%H-%M-%S")
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

        for take in args.take_count:
            for task in args.tasks:
                results = []
                try:
                    # don't use pool.apply_async, it's triggers http://bugs.python.org/issue10332
                    for result in pool.imap_unordered(driver, ( ( module, task, size, take, args.edit_count, seed )
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
                    with gzip.open(os.path.join(folder, "%s-%02d-%04d.json.gz" % ( task, take, len(results) )), "w") as jsonfile:
                        json.dump(results, jsonfile, indent=4, separators=( ",", ":" ))

    print>>sys.stderr, "Generating summary in %s ..." % ( folder, )
    files = sorted(set(chain.from_iterable( ( file for file in os.listdir(path) if file.endswith(".json.gz") ) for path in folders )))

    with open(os.path.join(folder, "summary.html"), "w") as htmlfile:
        print>>htmlfile, "<!doctype html>"
        print>>htmlfile, "<meta charset=utf-8>"
        print>>htmlfile, "<style>figure.inline-figure { display: inline-block; margin: 0; }</style>"

        styles = defaultdict(( { "color": color, "linestyle": linestyle, "marker": marker, "markersize": markersize }
            for color, ( linestyle, ( marker, markersize ) ) in izip(
                cycle(( "black", "darkblue", "darkred", "darkgreen", "darkcyan", "dimgray" )),
                cycle(product(( "-", "--", "-." ), ( ( ".", 8 ), ( "^", 6 ), ( "s", 4 ), ( "*", 7 ), ( "D", 4 ) ))) )).next)

        for file in files:
            print>>sys.stderr, "    Summarizing %s ..." % ( file, )
            label = file[:-8]
            summary = os.path.join(folder, label)
            print>>htmlfile, "<h1>%s</h1>" % ( label, )

            try:
                os.makedirs(summary)
            except OSError as e:
                if e.errno != errno.EEXIST:
                    raise

            results = []
            for path in folders:
                filepath = os.path.join(path, file)
                print>>sys.stderr, "        Loading %s ..." % ( filepath, ),
                try:
                    results.extend(json.load(gzip.open(filepath), object_pairs_hook=OrderedDict))
                except IOError as e:
                    if e.errno != errno.ENOENT:
                        raise
                    print>>sys.stderr, " not found"
                except Exception:
                    traceback.print_exc()
                else:
                    print>>sys.stderr, " done"

            table = {
                "time": {},
                "heap": {},
                "max-heap": {},
            }
            units = {}
            editables = set()
            for record in results:
                try:
                    table["time"].setdefault("from-scratch", {}).setdefault(record["module"], {}) \
                        .setdefault(record["size"], []).append(record["setup"]["time"])
                    table["heap"].setdefault("from-scratch", {}).setdefault(record["module"], {}) \
                        .setdefault(record["size"], []).append(record["setup"]["heap"])
                    table["max-heap"].setdefault("from-scratch", {}).setdefault(record["module"], {}) \
                        .setdefault(record["size"], []).append(record["setup"]["max-heap"])
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
                            table["time"].setdefault("propagate", {}).setdefault(record["module"], {}) \
                                .setdefault(record["size"], []).append(record["edits"]["update-time"] + record["edits"]["take-time"])
                            table["heap"].setdefault("propagate", {}).setdefault(record["module"], {}) \
                                .setdefault(record["size"], []).append(record["edits"]["update-heap"] + record["edits"]["take-heap"])
                            table["max-heap"].setdefault("propagate", {}).setdefault(record["module"], {}) \
                                .setdefault(record["size"], []).append(record["edits"]["max-heap"])
                            table["time"].setdefault("update", {}).setdefault(record["module"], {}) \
                                .setdefault(record["size"], []).append(record["edits"]["update-time"])
                        except Exception:
                            traceback.print_exc()
                            if "error" in record:
                                pprint.pprint(dict(record))

            from matplotlib.backends.backend_agg import FigureCanvasAgg as FigureCanvas
            from matplotlib.figure import Figure
            from matplotlib.ticker import FormatStrFormatter
            from scipy import stats

            xmax = {}
            ymax = {}
            for measurement, measurement_table in table.iteritems():
                xmax[measurement] = {}
                ymax[measurement] = {}
                for timing, module_table in measurement_table.iteritems():
                    for module, sizes in module_table.iteritems():
                        for size, values in sizes.iteritems():
                            avg = stats.tmean(values)
                            sizes[size] = avg
                            xmax[measurement][timing] = max(xmax[measurement].get(timing, 0), size)
                            ymax[measurement][timing] = max(ymax[measurement].get(timing, 0), avg)
                        module_table[module] = OrderedDict(sorted(sizes.iteritems()))
                    measurement_table[timing] = OrderedDict(
                        sorted(module_table.iteritems(), key=lambda ( module, sizes ): max(sizes.itervalues()), reverse=True))

            scalings = {
                "time": ( ( 1.01, ( "from-scratch", "propagate" ) ), (  1.01, ( "propagate", ) ) ),
                "heap": ( ( 1.01, ( "from-scratch", "propagate" ) ), ),
                "max-heap": ( ( 1.01, ( "from-scratch", "propagate" ) ), ),
            }

            for measurement, measurement_table in table.iteritems():
                for yadjust, timings in scalings[measurement]:
                    print>>sys.stderr, "        Plotting %s (%.2fx of %s)" % ( measurement, yadjust, timings )
                    svgfilename = "%s-%s-%s-%s.svg" % ( label, measurement, yadjust, "-".join(timings) )
                    with open(os.path.join(summary, svgfilename), "w") as svgfile:
                        fig = FigureCanvas(Figure(figsize=( 3.5, 3 ))).figure
                        ax = fig.add_subplot(1, 1, 1,
                            xlim=( 0, 1.01 * max( xmax[measurement][timing] for timing in timings ) ),
                            ylim=( 0, yadjust * max( ymax[measurement][timing] for timing in timings ) ))

                        ax.set_xlabel("input size", fontsize=8)
                        ax.set_ylabel("%s (%s)" % ( measurement, units[measurement] ), fontsize=8)
                        for axis in ( ax.get_xaxis(), ax.get_yaxis() ):
                            axis.set_major_formatter(FormatStrFormatter("%.3g"))
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
                            for module, sizes in module_table.iteritems():
                                sizes, values = zip(*sizes.iteritems())
                                print>>sys.stderr, "            %50s ... %s" \
                                    % ( "%s (%s)" % ( module, timing ), " ".join( format(value, "9.3g") for value in values ) )
                                ax.plot(sizes, values, label="%s (%s)" % ( module, timing ), markeredgecolor="none", **styles[module, timing])

                        try:
                            ax.legend(loc="best", prop={ "size": 8 }, frameon=False, fancybox=False)
                        except TypeError:
                            ax.legend(loc="best", prop={ "size": 8 }, fancybox=False)

                        if hasattr(fig, "tight_layout"):
                            fig.tight_layout(pad=0.5)

                        fig.savefig(svgfile, format="svg")
                        print>>htmlfile, "<figure class=inline-figure><img src=%s></figure>" \
                            % ( os.path.join(label, urllib.pathname2url(svgfilename)), )


            for baseline in args.baselines:
                svgfilename = "%s-overhead.svg" % ( label, )
                with open(os.path.join(summary, svgfilename), "w") as svgfile:
                    fig = FigureCanvas(Figure(figsize=( 3.5, 3 ))).figure
                    ax = fig.add_subplot(1, 1, 1,
                        xlim=( 0, 1.01 * xmax["time"]["propagate"] ))
                    ax.set_title("Overhead: X (from-scratch) / %s (from-scratch)" % ( baseline, ), fontsize=8)
                    ax.set_xlabel("input size", fontsize=8)
                    for axis in ( ax.get_xaxis(), ax.get_yaxis() ):
                        axis.set_major_formatter(FormatStrFormatter("%.3g"))
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

                    print>>sys.stderr, "        Plotting overhead using baseline %s ..." % ( baseline, )
                    for module in editables:
                        sizes, overheads = zip(*( ( size, value / table["time"]["from-scratch"][baseline][size] ) \
                            for size, value in table["time"]["from-scratch"][module].iteritems() ))
                        print>>sys.stderr, "            %32s ... %s" % ( module, " ".join( format(overhead, "9.3g") for overhead in overheads ) )
                        ax.plot(sizes, overheads, label=module, markeredgecolor="none", **styles[module, "from-scratch"])

                    try:
                        ax.legend(loc="best", prop={ "size": 8 }, frameon=False, fancybox=False)
                    except TypeError:
                        ax.legend(loc="best", prop={ "size": 8 }, fancybox=False)

                    if hasattr(fig, "tight_layout"):
                        fig.tight_layout(pad=0.5)

                    fig.savefig(svgfile, format="svg")
                    print>>htmlfile, "<figure class=inline-figure><img src=%s></figure>" \
                        % ( os.path.join(label, urllib.pathname2url(svgfilename)), )


            for baseline in args.baselines:
                svgfilename = "%s-speedup.svg" % ( label, )
                with open(os.path.join(summary, svgfilename), "w") as svgfile:
                    fig = FigureCanvas(Figure(figsize=( 3.5, 3 ))).figure
                    ax = fig.add_subplot(1, 1, 1,
                        xlim=( 0, 1.01 * xmax["time"]["propagate"] ))
                    ax.set_title("Speed-up: %s (from-scratch) / X (propagate)" % ( baseline, ), fontsize=8)
                    ax.set_xlabel("input size", fontsize=8)
                    for axis in ( ax.get_xaxis(), ax.get_yaxis() ):
                        axis.set_major_formatter(FormatStrFormatter("%.3g"))
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

                    print>>sys.stderr, "        Plotting speed-up using baseline %s ..." % ( baseline, )
                    for module in editables:
                        sizes, speedups = zip(*( ( size, table["time"]["from-scratch"][baseline][size] / value ) \
                            for size, value in table["time"]["propagate"][module].iteritems() ))
                        print>>sys.stderr, "            %32s ... %s" % ( module, " ".join( format(speedup, "9.3g") for speedup in speedups ) )
                        ax.plot(sizes, speedups, label=module, markeredgecolor="none", **styles[module, "propagate"])

                    try:
                        ax.legend(loc="best", prop={ "size": 8 }, frameon=False, fancybox=False)
                    except TypeError:
                        ax.legend(loc="best", prop={ "size": 8 }, fancybox=False)

                    if hasattr(fig, "tight_layout"):
                        fig.tight_layout(pad=0.5)

                    fig.savefig(svgfile, format="svg")
                    print>>htmlfile, "<figure class=inline-figure><img src=%s></figure>" \
                        % ( os.path.join(label, urllib.pathname2url(svgfilename)), )


            for module in editables:
                print>>sys.stderr, "        Plotting %s details ..." % ( module, )
                svgfilename = "%s-%s-details.svg" % ( label, module )
                with open(os.path.join(summary, svgfilename), "w") as svgfile:
                    fig = FigureCanvas(Figure(figsize=( 3.5, 3 ))).figure
                    ax = fig.add_subplot(1, 1, 1,
                        xlim=( 0, 1.01 * xmax["time"]["propagate"] ))
                    ax.set_title("%s details" % ( module, ), fontsize=8)
                    ax.set_xlabel("input size", fontsize=8)
                    ax.set_ylabel("time (%s)" % ( units["time"], ), fontsize=8)
                    for axis in ( ax.get_xaxis(), ax.get_yaxis() ):
                        axis.set_major_formatter(FormatStrFormatter("%.3g"))
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
                        sizes, values = zip(*table["time"][timing][module].iteritems())
                        print>>sys.stderr, "            %24s ... %s" \
                            % ( timing, " ".join( format(value, "9.3g") for value in values ) )
                        ax.plot(sizes, values, label="%s" % ( timing, ), markeredgecolor="none", **styles[module, timing])

                    try:
                        ax.legend(loc="best", prop={ "size": 8 }, frameon=False, fancybox=False)
                    except TypeError:
                        ax.legend(loc="best", prop={ "size": 8 }, fancybox=False)

                    if hasattr(fig, "tight_layout"):
                        fig.tight_layout(pad=0.5)

                    fig.savefig(svgfile, format="svg")
                    print>>htmlfile, "<figure class=inline-figure><img src=%s></figure>" \
                        % ( os.path.join(label, urllib.pathname2url(svgfilename)), )