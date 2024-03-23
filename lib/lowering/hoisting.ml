open Base
open Lookuptree

let hoist_all_locals block =
  let rec aux acc stmts =
    match stmts with
    | [] -> acc
    | s :: stmts -> (
        match s with
        | LetDecl (ident, _) -> aux (ident :: acc) stmts
        | While (_, b) -> aux acc (b.stmts @ stmts)
        | If (_, b1, b2) -> aux acc (b1.stmts @ b2.stmts @ stmts)
        | _ -> aux acc stmts)
  in
  let result = aux [] block.stmts in
  List.rev result
