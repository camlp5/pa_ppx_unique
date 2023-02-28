(**pp -syntax camlp5r *)
(* pa_ppx_unique_runtime.ml,v *)

module Unique = struct
  type unique 'a = { id : int ; node : 'a } ;
  value ctr = ref 0 ;
  value unique node =
    let id = ctr.val in do {
      ctr.val := 1 + ctr.val ;
      { id = id ; node = node }
    } ;
end ;
