open Base
open Lookuptree
open Ir

type state = {
  label_generator : Label.Generator.t;
  blocks : basicblock list;
  (* Current basicblock *)
  cur_label : Label.label;
  cur_block : stmt list;
}

module Dsl = Pass_dsl.No_errors (struct
  type t = state
end)

open Dsl

let assemble_cur_block cur_label cur_block terminator =
  { label = cur_label; body = List.rev cur_block; terminator }

(* ----- State Helpers ------ *)

let emit_stmt stmt =
  let* s = get in
  set { s with cur_block = stmt :: s.cur_block }

let _generate_label f =
  let* s = get in
  let gen, lab = f s.label_generator in
  set { s with label_generator = gen } *> return lab

let make_label name = _generate_label (Label.Generator.make_label ~name)
let make_temp_label = _generate_label Label.Generator.make_temp_label

let switch_basic_block ~terminator ~next_label =
  let* s = get in
  let cur_block = assemble_cur_block s.cur_label s.cur_block terminator in
  set
    {
      s with
      blocks = cur_block :: s.blocks;
      cur_label = next_label;
      cur_block = [];
    }

(* ----- Pass ------ *)

let map_op = function
  | Parsetree.BinopPlus -> BinopIntPlus
  | BinopMinus -> BinopIntMinus
  | BinopMul -> BinopIntMul
  | BinopDiv -> BinopIntDiv
  | BinopMod -> BinopIntMod
  | BinopEq -> BinopIntEq
  | BinopNeq -> BinopIntNeq
  | BinopLt -> BinopIntLt
  | BinopGt -> BinopIntGt
  | BinopLte -> BinopIntLte
  | BinopGte -> BinopIntGte

let rec map_expr = function
  | Lookuptree.Constant (ConstantInt i) -> Ir.Constant (Ir.ConstantInt i)
  | Ident i -> Ident i
  | Binop (e1, op, e2) -> Binop (map_expr e1, map_op op, map_expr e2)
  | Builtin b -> Builtin (map_builtin b)
  | Call _ -> Printf.failwithf "Function calls are not supported" ()

and map_builtin = function
  | Lookuptree.Print e -> Print (map_expr e)
  | Lookuptree.Println e -> Println (map_expr e)
  | Lookuptree.Assert e -> Assert (map_expr e)

let rec pass_stmt = function
  | LetDecl (ident, expr) | Assign (Ident ident, expr) ->
      emit_stmt (Assign (LvalueIdent ident, map_expr expr))
  | Assign _ -> Printf.failwithf "Invalid assignment" ()
  | ExprStmt e -> emit_stmt (ExprStmt (map_expr e))
  | If (cond, then_b, else_b) -> pass_if cond then_b else_b
  | While (cond, body) -> pass_while cond body
  | Return e ->
      let* lab = make_label "after_return" in
      switch_basic_block ~terminator:(Return (map_expr e)) ~next_label:lab

and pass_stmts = many ~f:pass_stmt
and pass_block { stmts; _ } = pass_stmts stmts

and pass_if cond then_b else_b =
  (* ...
       br cond then_lab else_lab
     then_lab:
       ... then_b ...
       jump after_if
     else_lab:
       ... else_b ...
       jump after_if
     after_if:
       ...
  *)
  let* then_label = make_label "if_then" in
  let* else_label = make_label "if_else" in
  let* after_label = make_label "if_after" in
  switch_basic_block
    ~terminator:
      (CondBranch
         { cond = map_expr cond; if_true = then_label; if_false = else_label })
    ~next_label:then_label
  *> pass_block then_b
  *> switch_basic_block ~terminator:(Jump after_label) ~next_label:else_label
  *> pass_block else_b
  *> switch_basic_block ~terminator:(Jump after_label) ~next_label:after_label

and pass_while cond body =
  (* ...
        jump cond
     cond_label:
       br cond body_label after_label
     body_label:
       ... body ...
       jump cond
     after_label:
       ...
  *)
  let* cond_label = make_label "while_cond" in
  let* body_label = make_label "while_body" in
  let* after_label = make_label "while_after" in
  switch_basic_block ~terminator:(Jump cond_label) ~next_label:cond_label
  *> switch_basic_block
       ~terminator:
         (CondBranch
            {
              cond = map_expr cond;
              if_true = body_label;
              if_false = after_label;
            })
       ~next_label:body_label
  *> pass_block body
  *> switch_basic_block ~terminator:(Jump cond_label) ~next_label:after_label

let lower' ({ entry } : lookuptree) : ir =
  let locals = Hoisting.hoist_all_locals entry in
  let pass_parsetree =
    pass_block entry
    *> let* s = get in
       let cur_block =
         assemble_cur_block s.cur_label s.cur_block
           (Return (Constant (ConstantInt 0)))
       in
       let blocks = List.rev (cur_block :: s.blocks) in
       match blocks with
       | entry_block :: other_blocks ->
           let entry = { entry_block; blocks = other_blocks; locals } in
           return { entry }
       | _ -> assert false
  in
  let gen = Label.Generator.make "lo" in
  let gen, entry_label = Label.Generator.make_label gen ~name:"entry" in
  let init =
    {
      label_generator = gen;
      blocks = [];
      cur_block = [];
      cur_label = entry_label;
    }
  in
  let ir = run_pass pass_parsetree ~init in
  Ir_cleanup.cleanup ir

let lower lookuptree : ir Errors.compilation_result = Ok (lower' lookuptree)
