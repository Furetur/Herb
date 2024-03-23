open Base
open Stdio
open Errors

type compiler_options = {
  path : string;
  outpath : string option;
  only_parsetree : bool;
  only_lookuptree : bool;
  only_ir : bool;
}

let get_paths { path; outpath; _ } =
  let given_inpath = Fpath.v path in
  let given_outpath = Option.map outpath ~f:Fpath.v in
  let default_outpath = Fpath.set_ext "" given_inpath in
  let bin_outpath = Option.value ~default:default_outpath given_outpath in
  let ll_outpath = Fpath.set_ext ".ll" bin_outpath in
  (given_inpath, ll_outpath, bin_outpath)

type stop_compilation =
  | CompilationError of error
  | StopWithDiagnostics of string

type 'a compilation_status = ('a, stop_compilation) Result.t

let wrap (x : 'a compilation_result) : 'a compilation_status =
  match x with Ok x -> Ok x | Error err -> Error (CompilationError err)

let ( let* ) = Result.( >>= )

let maybe_print_and_stop x show_x is_enabled =
  if is_enabled then
    let msg = show_x x in
    Error (StopWithDiagnostics msg)
  else Ok ()

let maybe_print_parsetree_and_stop parsetree only_parsetree =
  maybe_print_and_stop parsetree Parsetree.show_parsetree only_parsetree

let maybe_print_lookuptree_and_stop lookuptree only_lookuptree =
  maybe_print_and_stop lookuptree Lookuptree.show_lookuptree only_lookuptree

let maybe_print_ir_and_stop ir only_ir =
  maybe_print_and_stop ir Ir_prettyprint.show_ir only_ir

let compile ({ only_parsetree; only_ir; only_lookuptree; _ } as options) =
  let inpath, ll_outpath, bin_outpath = get_paths options in
  (* Compile *)
  let result =
    let* parsetree = wrap (Parser.parse inpath) in
    let* _ = maybe_print_parsetree_and_stop parsetree only_parsetree in
    let* lookuptree = wrap (Lookup.lookup parsetree) in
    let* _ = maybe_print_lookuptree_and_stop lookuptree only_lookuptree in
    let ir = Lowering.lower lookuptree in
    let* _ = maybe_print_ir_and_stop ir only_ir in
    wrap (Ll_gen.gen ir ll_outpath bin_outpath)
  in
  match result with
  | Ok () -> Ok ()
  | Error (StopWithDiagnostics msg) ->
      print_endline msg;
      Ok ()
  | Error (CompilationError e) ->
      print_endline e;
      Error ()
