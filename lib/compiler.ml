open Base
open Stdio

let run_clang llpath outpath =
  (* TODO: remove this warning *)
  let cmd =
    "clang -Wno-override-module " ^ Fpath.to_string llpath ^ " -o " ^ Fpath.to_string outpath
  in
  let exitcode = Stdlib.Sys.command cmd in
  if not (exitcode = 0) then print_endline "Clang returned a non-zero exit code"

(* - API - *)

type compiler_options = {
  path : string;
  outpath : string option;
  only_parsetree : bool;
  only_ir : bool;
}

let compile { path; outpath; only_parsetree; only_ir } =
  let path = Fpath.v path in
  let ll_path =
    outpath |> Option.map ~f:Fpath.v
    |> Option.value ~default:(Fpath.set_ext ".ll" path)
  in
  let outpath = Fpath.set_ext "" ll_path in
  (* Compile *)
  let parsetree = Parser.parse path in
  if only_parsetree then (
    print_endline (Parsetree.show_parsetree parsetree);
    Ok ())
  else
    let ir = Lowering.lower parsetree in
    if only_ir then (
      print_endline (Ir_prettyprint.show_ir ir);
      Ok ())
    else
      let ll = Ll_gen.gen_module ir in
      let x = Ll_write.write_to_file ll ll_path in
      match x with
      | Error e ->
          print_endline e;
          Error ()
      | Ok _ ->
          run_clang ll_path outpath;
          Ok ()
