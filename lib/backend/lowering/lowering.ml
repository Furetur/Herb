open Base
open Typed_ast
open Lowered_tree
open Loc
open Typ

type state = Lowered_tree.stmt list

module P = Pass.NoErrors(struct
  type t = state
end)

open P

(* ----- Pass ----- *)

let pass_literal = function
  | TBool b -> LiteralBool b
  | TInt i -> LiteralInt i
  | TString s -> LiteralString s
  | _ -> failwith "Function literals are not supported"


let rec pass_builtin = function
  | TPrint e -> 
    let* e = pass_expr e in
    return (BuiltinPrint e)
  | TAssert e ->
    let* e = pass_expr e in
    return (BuiltinAssert e)

and pass_expr { value = {typ = type'; node = expr}; _} = match expr with
  | TLiteral lit -> return (Literal (pass_literal lit))
  | TBuiltin b ->
    let* b = pass_builtin b in
    return (Builtin b)
  | TExtern _ -> failwith "extern not supported here"
  | TIdent ident -> return (ident, type')
  | TAssign (e1, e2) ->
    let* e1 = pass_expr e1 in
    

