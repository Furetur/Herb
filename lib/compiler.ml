open Base
open Stdio

type compiler_options = {
  path : string;
  outpath : string option;
  only_parsetree : bool;
  only_ir : bool;
}

let get_paths { path; outpath; _ } =
  let given_inpath = Fpath.v path in
  let given_outpath = Option.map outpath ~f:Fpath.v in
  let default_outpath = Fpath.set_ext "" given_inpath in
  let bin_outpath = Option.value ~default:default_outpath given_outpath in
  let ll_outpath = Fpath.set_ext ".ll" bin_outpath in
  (given_inpath, ll_outpath, bin_outpath)

let compile ({ only_parsetree; only_ir; _ } as options) =
  let inpath, ll_outpath, bin_outpath = get_paths options in
  (* Compile *)
  let parsetree = Parser.parse inpath in
  if only_parsetree then (
    print_endline (Parsetree.show_parsetree parsetree);
    Ok ())
  else
    let ir = Lowering.lower parsetree in
    if only_ir then (
      print_endline (Ir_prettyprint.show_ir ir);
      Ok ())
    else
      let x = Ll_gen.gen ir ll_outpath bin_outpath in
      match x with
      | Error e ->
          print_endline e;
          Error ()
      | Ok () -> Ok ()
