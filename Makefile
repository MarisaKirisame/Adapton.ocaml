
VPATH = Source Test
OCAMLBUILD_FLAGS = -j 0 -tags "warn_error_A,debug"
OCAMLBUILD_DOCDIR = Documents/API
OCAMLBUILD_PRODUCTDIR = _product
OUNIT_FLAGS = -verbose
export OCAMLPATH := $(CURDIR)


# local configuration hook; include it if it exists
-include Makefile.local


.PHONY : all test clean repl check

all : lib

lib : check $(addprefix ocamlbuild//Adapton.,cmxa a cma cmi)

test : check ounit//runtestadapton.d.byte

.PRECIOUS : $(OCAMLBUILD_PRODUCTDIR)/runbenchmark%.py

$(OCAMLBUILD_PRODUCTDIR)/runbenchmark%.py : runbenchmark%.py
	cp $< $@

benchmark-% : check $(OCAMLBUILD_PRODUCTDIR)/runbenchmark%.py ocamlbuild//runbenchmark%.native
	ulimit -s hard && $(OCAMLBUILD_PRODUCTDIR)/runbenchmark$*.py $(BENCHMARK_FLAGS) $(BENCHMARK_FLAGS.$*)

resummarize-benchmark-% : check $(OCAMLBUILD_PRODUCTDIR)/runbenchmark%.py ocamlbuild//runbenchmark%.native
	$(OCAMLBUILD_PRODUCTDIR)/runbenchmark$*.py $(RESUMMARIZE_FLAGS) $(RESUMMARIZE_FLAGS.$*) \
		--resummarize $(RESUMMARIZE_DIRS) $(RESUMMARIZE_DIRS.$*)

clean : ocamlbuild//clean

repl : check ocamltop//runadapton.top

check : check-trailing-whitespace check-hg

check-trailing-whitespace :
	@echo "Check trailing whitespace ..."
	-! grep -Irn $(addprefix --exclude=,'*.rej') '[         ]\+$$' $(VPATH) Makefile* _tags myocamlbuild.ml

check-hg :
	hg id -nibtB 2>/dev/null || true
	hg qselect 2>/dev/null || true

include Makefile.rules
