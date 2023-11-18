open Base
open Printf
open Ir
open Label

let show_constant = function ConstantInt i -> Int.to_string i

let show_binop = function
  | BinopIntPlus -> "+"
  | BinopIntMinus -> "-"
  | BinopIntMul -> "*"
  | BinopIntDiv -> "/"
  | BinopIntMod -> "%"

let rec show_expr = function
  | Constant c -> show_constant c
  | Ident i -> i
  | Binop (e1, op, e2) ->
      sprintf "%s %s %s" (show_expr e1) (show_binop op) (show_expr e2)

let show_stmt = function
  | Assign (LvalueIdent i, e) -> sprintf "%s = %s" i (show_expr e)
  | ExprStmt e -> show_expr e

let show_terminator = function
  | Jump label -> sprintf "jump %s" (show_label label)
  | CondBranch { cond; if_true; if_false } ->
      sprintf "if %s (jump %s) else (jump %s)" (show_expr cond)
        (show_label if_true) (show_label if_false)
  | Return e -> sprintf "return %s" (show_expr e)

let show_basicblock { label; body; terminator } =
  let label = show_label label ^ ":\n" in
  let body = List.map ~f:show_stmt body @ [ show_terminator terminator ] in
  let body = body |> List.map ~f:(fun x -> "\t" ^ x) |> String.concat_lines in
  label ^ body

let show_func_body { locals; entry_block; blocks } =
  let locals = sprintf "locals: %s\n" (String.concat ~sep:", " locals) in
  let blocks =
    entry_block :: blocks |> List.map ~f:show_basicblock |> String.concat_lines
  in
  locals ^ blocks

let show_ir { entry } = show_func_body entry
