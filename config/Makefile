# Makefile,v
# Copyright (c) INRIA 2007-2017

RM=rm

NOT_OCAMLFIND=not-ocamlfind
LAUNCH=env TOP=$(TOP) ocamlfind camlp5-buildscripts/LAUNCH --
OCAMLFIND=ocamlfind
YAWRAP=ocamlfind camlp5-buildscripts/ya-wrap-ocamlfind
OCAMLTOPLEVEL=ocaml
CAMLP5LIB=$(shell $(OCAMLFIND) query camlp5)
MKCAMLP5=mkcamlp5
CAMLP5R=camlp5r -I $(CAMLP5LIB)
INCLUDES=
OCAMLCFLAGS= $(INCLUDES)
PACKAGES=unix,camlp5
PACKAGES := $(PACKAGES),camlp5.parser_quotations
#PACKAGES := $(PACKAGES),camlp5.quotations
PRPACKAGE = camlp5.pr_o

export PERL5LIB := $(TOP):$(PERL5LIB)

top: all

.mli.cmi:
	$(LAUNCH) $(YAWRAP) $(OCAMLFIND) ocamlc $(DEBUG) $(OCAMLCFLAGS) -package $(PACKAGES) -c $<

.ml.cmo:
#	$(LAUNCH) $(YAWRAP) $(NOT_OCAMLFIND) preprocess $(OCAMLCFLAGS) -package $(PACKAGES),$(PRPACKAGE) $< > $*_ppo.ml
#	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) $(OCAMLCFLAGS) -package $(PACKAGES) -c $*_ppo.ml
	$(LAUNCH) $(YAWRAP) $(OCAMLFIND) ocamlc $(DEBUG) $(OCAMLCFLAGS) -package $(PACKAGES) -c $<

.ml.cmx:
	$(LAUNCH) $(YAWRAP) $(OCAMLFIND) ocamlopt $(DEBUG) $(OCAMLCFLAGS) -package $(PACKAGES) -c $<

.ml.cmt:
	@mkdir -p .cmt
	$(OCAMLFIND) ocamlc $(DEBUG) $(OCAMLCFLAGS) -package $(PACKAGES),pa_ppx_dock -bin-annot -c $< -o .cmt/$*.cmo
	cp .cmt/$*.cmt $*.cmt

.mli.cmti:
	@mkdir -p .cmt
	$(OCAMLFIND) ocamlc $(DEBUG) $(OCAMLCFLAGS) -package $(PACKAGES),pa_ppx_dock -bin-annot -c $< -o .cmt/$*.cmi
	cp .cmt/$*.cmti $*.cmti

.SUFFIXES: .cmo .cmi .ml .mli .cmx .cma .cmxa .cmt .cmti

clean::
	$(RM) -rf *.cm* *.o *.a *.byte *.opt *.log *.cache *.ppo *ppo.ml _build/* .cmt *.odoc

realclean:: clean
