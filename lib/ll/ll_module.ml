module M = Ollvm.Ez.Module
open Ir
open Ll_symtab

(* TODO: this function should accept the module's name or its descriptor or whatever  *)
let prepare_module () =
  let m = M.init "name" ("", "", "") "" in
  let m, builtins = Ll_builtins.add_builtin_declarations m in
  let symtab = { builtins } in
  (m, symtab)

let gen_module { entry } =
  let m, msybtab = prepare_module () in
  let m = Ll_func.gen_entry m msybtab entry in
  m
