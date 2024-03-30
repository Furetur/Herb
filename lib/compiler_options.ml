open Pass_dsl.Result_dsl_syntax

type expected_compilation_result = Parsetree | Lookuptree | Ir | Binary
[@@deriving show]

type compiler_options = {
  inpath : Fpath.t;
  ll_outpath : Fpath.t;
  bin_outpath : Fpath.t;
  expected_compilation_result : expected_compilation_result;
}

let parse_path s =
  match Fpath.of_string s with Ok x -> Ok x | Error (`Msg x) -> Error x

let deduce_outpaths inpath bin_outpath =
  let make_ll_outpath bin_outpath = Fpath.set_ext ".ll" bin_outpath in
  match bin_outpath with
  | None ->
      let bin_outpath = Fpath.set_ext "" inpath in
      let ll_outpath = make_ll_outpath bin_outpath in
      return (ll_outpath, bin_outpath)
  | Some bin_outpath ->
      let* bin_outpath = parse_path bin_outpath in
      return (make_ll_outpath bin_outpath, bin_outpath)

let make_compiler_options ~inpath ~bin_outpath ~expected_compilation_result =
  let* inpath = parse_path inpath in
  let* ll_outpath, bin_outpath = deduce_outpaths inpath bin_outpath in
  return { inpath; ll_outpath; bin_outpath; expected_compilation_result }
