
VPATH = Source $(wildcard Applications/*) Test $(wildcard Benchmarks/*)
ifeq ($(shell uname),Darwin)
	# bump stack size to 4GiB
	OCAMLBUILD_DARWIN_FLAGS := -lflag -cclib -lflag -Wl,-stack_size,0x100000000
endif
OCAMLBUILD_FLAGS = -j 0 -tags "warn_error_A,debug" -pkg num -use-menhir $(OCAMLBUILD_DARWIN_FLAGS)
OCAMLBUILD_DOCDIR = Documents/API
OCAMLBUILD_PRODUCTDIR = _product
OUNIT_FLAGS = -verbose
export OCAMLPATH := $(CURDIR)
export OCAMLFIND_IGNORE_DUPS_IN := Source


# local configuration hook; include it if it exists
-include Makefile.local


.PHONY : all test clean repl check

all : lib as2 adaptime

lib : check $(addprefix ocamlbuild//Adapton.,cmxa a cma cmi)

as2 : check ocamlbuild//runas2.native

adaptime : check $(addprefix ocamlbuild//Adaptime.,cmxa a cma cmi)

adaptime-test : check ounit//adaptimetest.native

test : check ounit//runtestadapton.d.byte

.PRECIOUS : $(OCAMLBUILD_PRODUCTDIR)/runbenchmark%.py

$(OCAMLBUILD_PRODUCTDIR)/runbenchmark%.py : runbenchmark%.py
	cp $< $@

benchmark-% : check $(OCAMLBUILD_PRODUCTDIR)/runbenchmark%.py ocamlbuild//runbenchmark%.native
	ulimit -s hard > /dev/null 2>&1 || ulimit -s unlimited; \
		$(OCAMLBUILD_PRODUCTDIR)/runbenchmark$*.py benchmark \
			$(and $(shell hg id 2>/dev/null),--label "r$(shell (hg id -n -rqparent && hg qapplied --config defaults.qapplied= || hg id -n) 2>/dev/null)") \
			$(BENCHMARK_FLAGS) $(BENCHMARK_FLAGS.$*)

resummarize-benchmark-% : check $(OCAMLBUILD_PRODUCTDIR)/runbenchmark%.py ocamlbuild//runbenchmark%.native
	$(OCAMLBUILD_PRODUCTDIR)/runbenchmark$*.py resummarize \
		$(RESUMMARIZE_FLAGS) $(RESUMMARIZE_FLAGS.$*) \
		$(and $(RESUMMARIZE_INPUTS.$*),--inputs $(RESUMMARIZE_INPUTS.$*)) \
		$(and $(RESUMMARIZE_OUTPUT.$*),--output $(RESUMMARIZE_OUTPUT.$*))

clean : ocamlbuild//clean

repl : check ocamltop//runadapton.top

check : check-trailing-whitespace check-hg

check-trailing-whitespace :
	@echo "Check trailing whitespace ..."
	-! grep -Irn $(addprefix --exclude=,'*.rej' '*.orig') '[         ]\+$$' $(VPATH) Makefile* _tags myocamlbuild.ml

check-hg :
	hg id -nibtB 2>/dev/null || true
	hg qselect 2>/dev/null || true

exhaustive : all $(addprefix ocamlbuild//,runadapton.top runtestadapton.d.byte runbenchmarkadapton.native adaptimetest.native)

# designed to run on an early 2009 Mac Pro with two 2.26 GHz quad-core Intel Xeons and 16 GB memory
pldi2014-benchmarks : SEEDS = -S 1 2 3 4 5 6 7 8
pldi2014-benchmarks : MODULES = -m "Adapton" "EagerTotalOrder"
pldi2014-benchmarks : BASELINES = -b "EagerNonInc" "LazyNonInc"
pldi2014-benchmarks : FLAGS = -s -N $(SEEDS) $(MODULES) $(BASELINES)
pldi2014-benchmarks : SUMMARIES = -s table $(BASELINES)
pldi2014-benchmarks : BENCHMARK = $(MAKE) benchmark-adapton
pldi2014-benchmarks : RESUMMARIZE = $(MAKE) resummarize-benchmark-adapton
pldi2014-benchmarks : OUTDIR := $(CURDIR)/Results/BenchmarkAdapton/pldi2014-$(shell date "+%F-%H-%M-%S")
pldi2014-benchmarks :
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/lazy" -t filter map -I 1000000 -T 1 -M -P 8 $(FLAGS)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/lazy" -t quicksort -I 100000 -T 1 -M -P 4 $(FLAGS)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/lazy" -t mergesort -I 100000 -T 1 -M -P 2 $(FLAGS)'
	-$(RESUMMARIZE) RESUMMARIZE_FLAGS='-I "$(OUTDIR)/lazy"/* -O "$(OUTDIR)/lazy/summary" $(SUMMARIES)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/batch" -t filter map -I 1000000 -T 1000001 -M -P 8 $(FLAGS)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/batch" -t exptree "tfold(sum)" "tfold(min)" -I 1000000 -T 1 -M -P 8 $(FLAGS)'
	-$(RESUMMARIZE) RESUMMARIZE_FLAGS='-I "$(OUTDIR)/batch"/* -O "$(OUTDIR)/batch/summary" $(SUMMARIES)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/switch" -t filter map -I 1000000 -T 1000001 -P 4 $(FLAGS)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/switch" -t "tfold(sum)" "tfold(min)" -I 1000000 -T 1 -P 2 $(FLAGS)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/switch" -t exptree -I 1000000 -T 1 -P 8 $(FLAGS)'
	-$(RESUMMARIZE) RESUMMARIZE_FLAGS='-I "$(OUTDIR)/switch"/* -O "$(OUTDIR)/switch/summary" $(SUMMARIES)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/swap" -t updown1 updown2 -I 40000 -T 1 -P 2 $(FLAGS)'
	-$(RESUMMARIZE) RESUMMARIZE_FLAGS='-I "$(OUTDIR)/swap"/* -O "$(OUTDIR)/swap/summary" $(SUMMARIES)'

pldi2014-trend-benchmarks :
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/trend" -t quicksort -I 100000 -T 5000 4500 4000 3500 3000 2500 2000 1500 1000 500 200 100 50 20 10 5 2 1 -M $(SEEDS) -P 4 -m "Adapton" "EagerTotalOrder" "LazyNonInc" -b "EagerNonInc" -s plots'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/trend" -t mergesort -I 100000 -T 5000 4500 4000 3500 3000 2500 2000 1500 1000 500 200 100 50 20 10 5 2 1 -M $(SEEDS) -P 4 -m "Adapton" "LazyNonInc" -b "EagerNonInc" -s plots'

# small version of pldi2014-benchmarks with 10% of the input size and not parallel
small-pldi2014-benchmarks : SEEDS = -S 1 2 3 4 5 6 7 8
small-pldi2014-benchmarks : MODULES = -m "Adapton" "EagerTotalOrder" -b "EagerNonInc" "LazyNonInc"
small-pldi2014-benchmarks : BASELINES = -b "EagerNonInc" "LazyNonInc"
small-pldi2014-benchmarks : FLAGS = -s -N $(SEEDS) $(MODULES) $(BASELINES)
small-pldi2014-benchmarks : SUMMARIES = -s table $(BASELINES)
small-pldi2014-benchmarks : BENCHMARK = $(MAKE) benchmark-adapton
small-pldi2014-benchmarks : RESUMMARIZE = $(MAKE) resummarize-benchmark-adapton
small-pldi2014-benchmarks : OUTDIR := $(CURDIR)/Results/BenchmarkAdapton/small-pldi2014-$(shell date "+%F-%H-%M-%S")
small-pldi2014-benchmarks :
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/lazy" -t filter map -I 100000 -T 1 -M -P 1 $(FLAGS)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/lazy" -t quicksort -I 10000 -T 1 -M -P 1 $(FLAGS)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/lazy" -t mergesort -I 10000 -T 1 -M -P 1 $(FLAGS)'
	-$(RESUMMARIZE) RESUMMARIZE_FLAGS='-I "$(OUTDIR)/lazy"/* -O "$(OUTDIR)/lazy/summary" $(SUMMARIES)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/batch" -t filter map -I 100000 -T 100001 -M -P 1 $(FLAGS)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/batch" -t exptree "tfold(sum)" "tfold(min)" -I 100000 -T 1 -M -P 1 $(FLAGS)'
	-$(RESUMMARIZE) RESUMMARIZE_FLAGS='-I "$(OUTDIR)/batch"/* -O "$(OUTDIR)/batch/summary" $(SUMMARIES)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/switch" -t filter map -I 100000 -T 100001 -P 1 $(FLAGS)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/switch" -t "tfold(sum)" "tfold(min)" -I 100000 -T 1 -P 1 $(FLAGS)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/switch" -t exptree -I 100000 -T 1 -P 1 $(FLAGS)'
	-$(RESUMMARIZE) RESUMMARIZE_FLAGS='-I "$(OUTDIR)/switch"/* -O "$(OUTDIR)/switch/summary" $(SUMMARIES)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/swap" -t updown1 updown2 -I 4000 -T 1 -P 1 $(FLAGS)'
	-$(RESUMMARIZE) RESUMMARIZE_FLAGS='-I "$(OUTDIR)/swap"/* -O "$(OUTDIR)/swap/summary" $(SUMMARIES)'

small-pldi2014-trend-benchmarks :
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/trend" -t quicksort -I 10000 -T 500 450 400 350 300 250 200 150 100 50 20 10 5 2 1 -M $(SEEDS) -P 1 -m "Adapton" "EagerTotalOrder" "LazyNonInc" -b "EagerNonInc" -s plots'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/trend" -t mergesort -I 10000 -T 500 450 400 350 300 250 200 150 100 50 20 10 5 2 1 -M $(SEEDS) -P 1 -m "Adapton" "LazyNonInc" -b "EagerNonInc" -s plots'

include Makefile.rules
