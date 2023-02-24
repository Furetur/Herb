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

let make_import raw : import t = located raw

let make_entry block : parsed_top t =
 fun loc -> PTopEntry { value = block; loc }

let make_top_level_let raw : parsed_top t =
 fun loc -> PTopDecl { loc; value = AToplevelLet raw }

let make_extern name typ linkname loc =
  PTopDecl { loc; value = AExtern { name; typ; linkname } }

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

let make_for i (start_ : expr) (end_ : expr) (body : block) : expr t =
 fun loc ->
  let id = bad_locate ~value:(AIdent i) in
  let one = bad_locate ~value:(ALiteral (AInt 1)) in
  let increment_i =
    bad_locate
      ~value:
        (AExprStmt (AAssign (id, bad_locate ~value:(ABinOp (id, APlus, one)))))
  in
  locate loc
    ~value:
      (ABlock
         [
           bad_locate ~value:(ALetStmt (i, start_));
           locate loc
             ~value:
               (AExprStmt
                  (AWhile
                     {
                       cond = bad_locate ~value:(ABinOp (id, ALte, end_));
                       body =
                         [
                           bad_locate ~value:(AExprStmt (ABlock body));
                           increment_i;
                         ];
                     }));
         ])

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

let make_or l r : expr t =
 fun loc ->
  let true_ = ALiteral (ABool true) in
  locate loc
    ~value:
      (AIf
         {
           cond = l;
           then_ = [ bad_locate ~value:(AExprStmt true_) ];
           else_ = [ expr_to_stmt r ];
         })

let make_and l r : expr t =
 fun loc ->
  let false_ = ALiteral (ABool false) in
  locate loc
    ~value:
      (AIf
         {
           cond = l;
           then_ = [ expr_to_stmt r ];
           else_ = [ bad_locate ~value:(AExprStmt false_) ];
         })
