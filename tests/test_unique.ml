(** -syntax camlp5o $(IMPORT_OCAMLCFLAGS) *)
(* test_unique.ml *)

type 'a id = 'a
and 'a option = 'a Option.t = None | Some of 'a
and term_id = term id
and term_option = term option
and term =
    Ref of int
  | Abs of term_id
  | App of term * term
  | Foo of term_option
[@@deriving unique { uniqified_module_name = LAMH
(*
                   ; normal_module_name = LAM
*)
                     ; skip_types = [
                         id
                       ]
                     }]
;;

module XX = struct

type term =
    Ref of int
  | Abs of term
  | App of term * term
  | Foo of term Option.t
[@@deriving unique { uniqified_module_name = LAM2H
(*
                     ; normal_module_name = LAM2
*)
                     ; pertype_customization = {
                         term = {
                           unique_constructor = term
                         }
                       }
                     }]
end
;;


type variable = int (* 1..max_var *) ;;

module BDD = struct
type bdd = Zero | One | Node of variable * bdd (*low*) * bdd (*high*)
end

type bdd = BDD.bdd = Zero | One | Node of variable * bdd (*low*) * bdd (*high*)
[@@deriving unique { uniqified_module_name = UN
                     ; normal_module_name = OK
                     }]
;;

[%%import: MLast.expr
    [@with
       loc := Ploc.t ;
       type_var := MLast.type_var ;
    ]
]
[@@deriving unique { uniqified_module_name = ASTH
                     ; normal_module_name = AST
                     ; skip_types = [
                         longid_lident
                       ; attribute
                       ; attributes_no_anti
                       ; attributes
                       ]
                     }]
;;
