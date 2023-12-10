open Base
module V = Ollvm.Ez.Value

(* ----- Module Symtab ----- *)

type module_symtab = { builtins : Ll_builtins.builtins_symtab }

(* ----- Function Symtab ----- *)

type locals_symtab = (Ir.ident, V.t, String.comparator_witness) Map.t

type labels_symtab =
  (Label.label, V.t, Label.Comparator.comparator_witness) Map.t

type func_symtab = { locals : locals_symtab; labels : labels_symtab }

let lookup_local fsymtab ident = Map.find_exn fsymtab.locals ident
let lookup_label fsymtab ident = Map.find_exn fsymtab.labels ident
