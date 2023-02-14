# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile

DESTDIR=
RM=rm

PACKAGES := $(PACKAGES),pa_ppx.deriving,pa_ppx.deriving_plugins.show,pa_ppx.base,pa_ppx.unmatched_vala,pa_ppx.deriving_plugins.params

ML=\
	pa_deriving_unique.ml pa_ppx_unique_runtime.ml \

TOOLSML = mk_meta.ml
CMO := $(ML:.ml=.cmo)
CMI := $(ML:.ml=.cmi)
CMX := $(ML:.ml=.cmx)
CMT := $(ML:.ml=.cmt)
CMTI := $(MLI:.mli=.cmti)

all: $(CMO) $(CMX) local-install

doc: $(CMT) $(CMTI)

KITS=$(CMO)

META: mk_meta
	./mk_meta > META

mk_meta: mk_meta.cmo
	$(OCAMLFIND) ocamlc -linkpkg -linkall -package fmt $< -o $@

mk_meta.ml: mk_meta.ML
	cppo $< > $@

install:: META
	mkdir -p $(DESTDIR)/lib
	$(NOT_OCAMLFIND) reinstall-if-diff pa_ppx_unique -destdir $(DESTDIR)/lib META $(CMO) $(CMI) $(CMX) $(MLI:.mli=.cmi) $(KITS:.cmo=.o) $(wildcard *.cmt*)

local-install::
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

clean::
	rm -rf META mk_meta mk_meta.ml

EXTERNAL := $(shell $(OCAMLFIND) query -predicates byte -format '%m' $(PACKAGES) | grep local-install)
$(CMO) $(CMI) $(CMX): $(EXTERNAL)

depend::
	echo "$(CMO) $(CMI) $(CMX): $(EXTERNAL)" > .depend.NEW
	$(LAUNCH) $(YAWRAP) $(OCAMLFIND) ocamldep $(DEBUG) -package $(PACKAGES) -- $(ML) $(TOOLSML) $(MLI) >> .depend.NEW \
		&& mv .depend.NEW .depend

-include .depend
