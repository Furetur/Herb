open Base
open Stdio
module P = Ollvm.Printer
module M = Ollvm.Ez.Module

(* - API - *)

type compiler_options = {
  path : string;
  outpath : string option;
  only_parsetree : bool;
}

let compile { path; outpath; only_parsetree } =
  let path = Fpath.v path in
  let outpath =
    outpath |> Option.map ~f:Fpath.v
    |> Option.value ~default:(Fpath.set_ext ".exe" path)
  in
  (* Compile *)
  let parsetree = Parser.parse path in
  if only_parsetree then print_endline (Parsetree.show_parsetree parsetree);
  Ok outpath
