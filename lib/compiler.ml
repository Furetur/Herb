open Base
open Stdio

(* - API - *)

type compiler_options = {
  path : string;
  outpath : string option;
  only_parsetree : bool;
  only_ir : bool;
}

let compile { path; outpath; only_parsetree; only_ir } =
  let path = Fpath.v path in
  let outpath =
    outpath |> Option.map ~f:Fpath.v
    |> Option.value ~default:(Fpath.set_ext ".ll" path)
  in
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
      Ll_write.write_to_file ll outpath
