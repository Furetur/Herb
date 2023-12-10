module V = Ollvm.Ez.Value
module M = Ollvm.Ez.Module
module B = Ollvm.Ez.Block
module T = Ollvm.Ez.Type
module I = Ollvm.Ez.Instr

type builtins_symtab = { print : V.t; println : V.t; assert' : V.t }

let add_func_decl m name arg_types ret_type =
  let m, name = M.global m ret_type name in
  let m = M.declaration m (B.declare name arg_types) in
  (m, name)

let add_print_decl m = add_func_decl m "print_int" [ T.i32 ] T.i32
let add_println_decl m = add_func_decl m "println_int" [ T.i32 ] T.i32
let add_assert_decl m = add_func_decl m "assert" [ T.i1 ] T.i32

let add_builtin_declarations m =
  let m, print = add_print_decl m in
  let m, println = add_println_decl m in
  let m, assert' = add_assert_decl m in
  (m, { print; println; assert' })
