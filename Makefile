
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
	ulimit -s hard && \
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
pldi2014-benchmarks : MODULES = -m "Adapton" "EagerTotalOrder" -b "EagerNonInc" "LazyNonInc"
pldi2014-benchmarks : FLAGS = $(SEEDS) $(MODULES)
pldi2014-benchmarks : BENCHMARK = $(MAKE) benchmark-adapton
pldi2014-benchmarks : OUTDIR := $(CURDIR)/Results/BenchmarkAdapton/pldi2014-$(shell date "+%F-%H-%M-%S")
pldi2014-benchmarks :
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/lazy" -L "monotonic-take-1" -t filter map -I 1000000 4 -T 1 -M -P 8 $(FLAGS)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/lazy" -L "monotonic-take-1" -t quicksort -I 100000 4 -T 1 -M -P 4 $(FLAGS)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/lazy" -L "monotonic-take-1" -t mergesort -I 100000 4 -T 1 -M -P 2 $(FLAGS)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/batch" -L "monotonic-take-all" -t filter map -I 1000000 4 -T 1000001 -M -P 8 $(FLAGS)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/batch" -L "monotonic-take-all" -t exptree "tfold(sum)" "tfold(min)" -I 1000000 4 -T 1 -M -P 8 $(FLAGS)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/switch" -L "non-monotonic-take-all" -t filter map -I 1000000 4 -T 1000001 -P 4 $(FLAGS)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/switch" -L "non-monotonic-take-all" -t "tfold(sum)" "tfold(min)" -I 1000000 4 -T 1 -P 2 $(FLAGS)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/switch" -L "non-monotonic-take-all" -t exptree -I 1000000 4 -T 1 -P 8 $(FLAGS)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/swap" -L "non-monotonic-take-all" -t updown1 updown2 -I 100000 4 -T 1 -P 2 $(FLAGS)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/trend" -L "monotonic-trend" -t quicksort -I 100000 -T 5000 4500 4000 3500 3000 2500 2000 1500 1000 500 200 100 50 20 10 5 2 1 -M $(SEEDS) -P 4 -m "Adapton" "EagerTotalOrder" "LazyNonInc" -b "EagerNonInc"'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/trend" -L "monotonic-trend" -t mergesort -I 100000 -T 5000 4500 4000 3500 3000 2500 2000 1500 1000 500 200 100 50 20 10 5 2 1 -M $(SEEDS) -P 4 -m "Adapton" "LazyNonInc" -b "EagerNonInc"'

# small version of pldi2014-benchmarks with 10% of the input size and not parallel
small-pldi2014-benchmarks : SEEDS = -S 1 2 3 4 5 6 7 8
small-pldi2014-benchmarks : MODULES = -m "Adapton" "EagerTotalOrder" -b "EagerNonInc" "LazyNonInc"
small-pldi2014-benchmarks : FLAGS = $(SEEDS) $(MODULES)
small-pldi2014-benchmarks : BENCHMARK = $(MAKE) benchmark-adapton
small-pldi2014-benchmarks : OUTDIR := $(CURDIR)/Results/BenchmarkAdapton/small-pldi2014-$(shell date "+%F-%H-%M-%S")
small-pldi2014-benchmarks :
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/lazy" -L "monotonic-take-1" -t filter map -I 100000 4 -T 1 -M -P 1 $(FLAGS)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/lazy" -L "monotonic-take-1" -t quicksort -I 10000 4 -T 1 -M -P 1 $(FLAGS)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/lazy" -L "monotonic-take-1" -t mergesort -I 10000 4 -T 1 -M -P 1 $(FLAGS)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/batch" -L "monotonic-take-all" -t filter map -I 100000 4 -T 100001 -M -P 1 $(FLAGS)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/batch" -L "monotonic-take-all" -t exptree "tfold(sum)" "tfold(min)" -I 100000 4 -T 1 -M -P 1 $(FLAGS)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/switch" -L "non-monotonic-take-all" -t filter map -I 100000 4 -T 100001 -P 1 $(FLAGS)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/switch" -L "non-monotonic-take-all" -t "tfold(sum)" "tfold(min)" -I 100000 4 -T 1 -P 1 $(FLAGS)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/switch" -L "non-monotonic-take-all" -t exptree -I 100000 4 -T 1 -P 1 $(FLAGS)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/swap" -L "non-monotonic-take-all" -t updown1 updown2 -I 10000 4 -T 1 -P 1 $(FLAGS)'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/trend" -L "monotonic-trend" -t quicksort -I 10000 -T 500 450 400 350 300 250 200 150 100 50 20 10 5 2 1 -M $(SEEDS) -P 1 -m "Adapton" "EagerTotalOrder" "LazyNonInc" -b "EagerNonInc"'
	-$(BENCHMARK) BENCHMARK_FLAGS='-O "$(OUTDIR)/trend" -L "monotonic-trend" -t mergesort -I 10000 -T 500 450 400 350 300 250 200 150 100 50 20 10 5 2 1 -M $(SEEDS) -P 1 -m "Adapton" "LazyNonInc" -b "EagerNonInc"'

include Makefile.rules
