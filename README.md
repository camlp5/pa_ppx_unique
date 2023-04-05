A PPX Rewriter for Uniqifying an AST

### Version

This is ``pa_ppx_unique`` (alpha) version 0.10.

# Overview

When we define an AST, and write code over it, typically we don't (at
first) insert unique IDs in the AST; only later do we perhaps realise
that unique IDs would be useful, and at that point there's a lot of
code that needs to be retrofitted.  But also, writing patterns and
expressions over ASTs with unique IDs is a PITA, and it might be nice
to automate as much as possible.

That's the goal of this PPX rewriter.  You supply an AST type that
doesn't have unique IDs in it, and it produces an AST type *with*
unique IDs in it.  And then, using `camlp5/pa_ppx_migrate` you can
generate functions back-and-forth, and using `camlp5/pa_ppx_q_ast` you
can generate quotations over this new type with unique IDs, so that
your code can be nearly-indifferent to those unique IDs.

# Usage

This rewriter is a deriver (in the style of [PPX Deriving](https://github.com/ocaml-ppx/ppx_deriving),
but implemented using [Camlp5](https://github.com/camlp5/camlp5)
and [pa_ppx](https://github.com/chetmurthy/pa_ppx).  To use it, add
attributes to type-declarations, e.g. (from `tests/test_unique.ml`)
```
type term =
    Ref of int
  | Abs of term
  | App of term * term[@@unique_constructor term]
[@@deriving unique { uniqified_module_name = LAM
                     }]
```
and compile with this package
```
ocamlfind ocamlc -c -package pa_ppx_unique -syntax camlp5o  -i test_unique.ml
```

Almost all the examples are all with a single type, but there is an
example of the entire OCaml AST from Camlp5: which comprises a large
number of mutually-recursive types.

# Usage Details

A description of the format of that `[@@deriving ...]` attribute:

1. `uniqified_module_name` is not optional, and is used to specify a module
   which will be created and in which all new types and value
   definitions will be placed.  This is necessary since the new types
   are very similar to the old ones, and we need to put them in a
   different structure.

2. `normal_module_name` is not optional, and specifies a module which
   will be created and filled with the normal version of the type
   (using type-equations so it's equal to the original type).  This is
   useful in the case where the types being uniqified are imported via
   `pa_ppx_import`.

3. `unique_constructor` is an item-attribute of each type, and
   specifies the name to be used for the smart-constructor of each
   type, that does uniqification.  It is optional, with default value
   the same as the type-name.
