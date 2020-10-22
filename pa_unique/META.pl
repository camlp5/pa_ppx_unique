#!/usr/bin/env perl

use strict ;
BEGIN { push (@INC, "..") }
use Version ;

our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for the "pa_ppx_unique" preprocessor:
version = "$Version::version"
description = "pa_ppx_unique deriver"

  package "runtime" (
    archive(byte) = "pa_ppx_unique_runtime.cmo"
    archive(native) = "pa_ppx_unique_runtime.cmx"
  )

  requires(toploop) = "camlp5,pa_ppx.deriving_plugins.show,pa_ppx.params_runtime"
  archive(toploop) = "pa_deriving_unique.cmo"

    requires(syntax,preprocessor) = "camlp5,pa_ppx.deriving_plugins.show,pa_ppx.params_runtime"
    archive(syntax,preprocessor,-native) = "pa_deriving_unique.cmo"
    archive(syntax,preprocessor,native) = "pa_deriving_unique.cmx"

  package "link" (
  requires(byte) = "camlp5,pa_ppx.deriving_plugins.show.link,pa_ppx.params_runtime"
  archive(byte) = "pa_deriving_unique.cmo"
  )
  requires = "pa_ppx_unique.runtime,camlp5,pa_ppx.deriving_plugins.show,pa_ppx.runtime,pa_ppx.params_runtime"

EOF
