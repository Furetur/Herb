open Base
open Stdio

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
      let x = Ll_gen.gen ir ll_path outpath in
      match x with
      | Error e ->
          print_endline e;
          Error ()
      | Ok () -> Ok ()
