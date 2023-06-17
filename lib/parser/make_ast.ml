open Base
open Loc
open Ast

(* ----- Parser helpers ----- *)
let bad_locate ~value = locate bad_loc ~value

type 'a t = loc -> 'a

let located x loc = { value = x; loc }
let expr_to_stmt { loc; value = expr } = locate loc ~value:(AExprStmt expr)

(* --- Types --- *)

let make_typ_named name : typ t = located (ATypNamed name)

let make_typ_fun farg_types ret_typ : typ t =
  located (ATypFun { farg_types; ret_typ })

(* --- Declarations --- *)

let make_raw_let_decl name e : raw_let_decl = (name, e)

(* - Top level - *)

let make_entry block : top_decl t = located (AEntry block)
let make_top_level_let raw : top_decl t = located (AToplevelLet raw)

let make_extern name typ linkname : top_decl t =
  located (AExtern { name; typ; linkname })

(* --- Statements --- *)

let make_let_stmt raw : stmt t = located (ALetStmt raw)
let make_expr_stmt { loc; value = expr } = { loc; value = AExprStmt expr }

(* --- Expressions --- *)

let make_ident (x : string) : expr t = located (AIdent x)

(* - Literals - *)

let make_string (s : string) : expr t = located (ALiteral (AString s))
let make_int (x : int) : expr t = located (ALiteral (AInt x))

let make_fun_literal fargs body : expr t =
  located (ALiteral (AFun { fargs; body }))

(* - Other - *)

let make_fun_call callee args = located (AFunCall { callee; args })
let make_block exprs : expr t = located (ABlock exprs)
let make_if cond then_ else_ : expr t = located (AIf { cond; then_; else_ })
let make_while cond body = located (AWhile { cond; body })
let make_unop op e : expr t = located (AUnOp (op, e))

(* - Binary operators - *)

let simple_binop op l r = located (ABinOp (l, op, r))
let make_assign l r : expr t = located (AAssign (l, r))
let make_eq = simple_binop AEq
let make_neq = simple_binop ANeq
let make_lt = simple_binop ALt
let make_lte = simple_binop ALte
let make_gt = simple_binop AGt
let make_gte = simple_binop AGte
let make_plus = simple_binop APlus
let make_minus = simple_binop AMinus
let make_mul = simple_binop AMul
let make_div = simple_binop ADiv
let make_mod = simple_binop AMod
let make_or = simple_binop AOr
let make_and = simple_binop AAnd
