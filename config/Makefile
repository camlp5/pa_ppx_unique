# Makefile,v
# Copyright (c) INRIA 2007-2017

RM=rm

OCAMLFIND=ocamlfind
NOT_OCAMLFIND=not-ocamlfind
OCAMLTOPLEVEL=ocaml
CAMLP5LIB=$(shell $(OCAMLFIND) query camlp5)
MKCAMLP5=mkcamlp5
CAMLP5R=camlp5r -I $(CAMLP5LIB)
INCLUDES=
OCAMLCFLAGS= $(INCLUDES)
PACKAGES=unix,camlp5
PACKAGES := $(PACKAGES),camlp5.parser_quotations
#PACKAGES := $(PACKAGES),camlp5.quotations
SYNTAX = camlp5r
PRPACKAGE = camlp5.pr_r

export PERL5LIB := $(TOP):$(PERL5LIB)

top: all

.mli.cmi:
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) $(OCAMLCFLAGS) -package $(PACKAGES) -syntax $(SYNTAX) -c $<

.ml.cmo:
#	$(NOT_OCAMLFIND) preprocess $(OCAMLCFLAGS) -package $(PACKAGES),$(PRPACKAGE) -syntax $(SYNTAX) $< > $*_ppo.ml
#	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) $(OCAMLCFLAGS) -package $(PACKAGES) -syntax $(SYNTAX) -c $*_ppo.ml
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) $(OCAMLCFLAGS) -package $(PACKAGES) -syntax $(SYNTAX) -c $<

.ml.cmx:
	$(OCAMLFIND) ocamlopt $(DEBUG) $(WARNERR) $(OCAMLCFLAGS) -package $(PACKAGES) -syntax $(SYNTAX) -c $<

.ml.cmt:
	@mkdir -p .cmt
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) $(OCAMLCFLAGS) -package $(PACKAGES),pa_ppx_dock -syntax $(SYNTAX) -bin-annot -c $< -o .cmt/$*.cmo
	cp .cmt/$*.cmt $*.cmt

.mli.cmti:
	@mkdir -p .cmt
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) $(OCAMLCFLAGS) -package $(PACKAGES),pa_ppx_dock -syntax $(SYNTAX) -bin-annot -c $< -o .cmt/$*.cmi
	cp .cmt/$*.cmti $*.cmti

.SUFFIXES: .cmo .cmi .ml .mli .cmx .cma .cmxa .cmt .cmti

clean::
	$(RM) -rf *.cm* *.o *.a *.byte *.opt *.log *.cache *.ppo *ppo.ml _build/* .cmt *.odoc

realclean:: clean
