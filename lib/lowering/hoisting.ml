open Base
open Parsetree

let hoist_all_locals block =
  let rec aux acc stmts =
    match stmts with
    | [] -> acc
    | s :: stmts -> (
        match s with
        | LetDecl (ident, _) -> aux (ident :: acc) stmts
        | While (_, b) -> aux acc (b @ stmts)
        | If (_, b1, b2) -> aux acc (b1 @ b2 @ stmts)
        | _ -> aux acc stmts)
  in
  let result = aux [] block in
  assert (not (List.contains_dup result ~compare:String.compare));
  List.rev result
