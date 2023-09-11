# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

DESTDIR=

PACKAGES := $(PACKAGES),oUnit,fmt,rresult,compiler-libs.common,sexplib0,uint,pa_ppx.utils,pa_ppx.import
PACKAGES := $(PACKAGES),pa_ppx_unique,pa_ppx_unique.runtime,pa_ppx_migrate
export IMPORT_OCAMLCFLAGS += -ppopt -pa_passthru-debug 
export IMPORT_OCAMLCFLAGS += -ppopt -pa_import-package -ppopt camlp5
export IMPORT_OCAMLCFLAGS += -ppopt -pa_import-I -ppopt .

TESTS= \
	test_unique.byte \

ML=\
	test_unique.ml

ALLSRC=$(ML)
ALLPACKAGES=$(PACKAGES)

all: $(TESTS) $(TESTS:.byte=.opt)

test: $(TESTS:.byte=)

% : %.opt
	mkdir -p _build && ./$<

test_unique.byte: test_unique.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES) \
	-linkpkg -syntax camlp5o $^ -o $@

test_unique.opt: test_unique.cmx
	$(LAUNCH) $(OCAMLFIND) ocamlopt $(DEBUG) $(WARNERR) -package $(PACKAGES) \
	-linkpkg -syntax camlp5o $^ -o $@

clean::
	rm -f  *.ppo.*
