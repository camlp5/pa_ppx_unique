#include "../version.inc"

let top = try Sys.getenv "TOP" with Not_found -> "" ;;
let destdir = ref (top ^ "/local-install") ;;

Arg.(parse [
         "-destdir", (Arg.String (fun s -> destdir := s)),
         ("<path>    Set the destination directory (default: " ^
	    !destdir ^ ")")
       ]
       (fun _ -> failwith "mk_meta: no anonymous args supported")
     "mk_meta -destdir <dir>")
;;

Fmt.(pf stdout {|# Specifications for the "pa_ppx_unique" preprocessor:
version = "%s"
description = "pa_ppx_unique deriver"

  package "runtime" (
    archive(byte) = "pa_ppx_unique_runtime.cmo"
    archive(native) = "pa_ppx_unique_runtime.cmx"
  )

  requires(toploop) = "camlp5,pa_ppx.deriving,pa_ppx.deriving_plugins.show,pa_ppx.params_runtime"
  archive(toploop) = "pa_deriving_unique.cmo"

    requires(syntax,preprocessor) = "camlp5,pa_ppx.deriving,pa_ppx.deriving_plugins.show,pa_ppx.params_runtime"
    archive(syntax,preprocessor,-native) = "pa_deriving_unique.cmo"
    archive(syntax,preprocessor,native) = "pa_deriving_unique.cmx"

  package "link" (
  requires = "camlp5,pa_ppx.deriving.link,pa_ppx.deriving_plugins.show.link,pa_ppx.params_runtime"
  archive(byte) = "pa_deriving_unique.cmo"
  archive(native) = "pa_deriving_unique.cmx"
  )

  requires = "pa_ppx_unique.runtime"
|} version)
