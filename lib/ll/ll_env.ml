open Base
open Unix

type ll_environment = {
  compiler_executable_path : Fpath.t;
  runtime_obj_path : Fpath.t;
} [@@deriving show]

let runtime_obj_file_name = "runtime.o"

let _assert_file_exists path file_type =
  let s_path = Fpath.to_string path in
  if not (Stdlib.Sys.file_exists s_path) then
    Printf.failwithf "%s does not exist: %s" file_type s_path ()

let _get_argv_0 () =
  let x = Array.get (Sys.get_argv ()) 0 in
  Fpath.v x

let _follow_symlink path =
  let str_path = Fpath.to_string path in
  let stats = lstat str_path in
  match stats.st_kind with
  | S_LNK ->
      let actual_s_rel_path = readlink str_path in
      Logs.debug (fun m ->
          m "Unix.readlink %s -> %s" str_path actual_s_rel_path);
      let actual_rel_path = Fpath.v actual_s_rel_path in
      (* `actual_rel_path` is relative to the symlink at `path` *)
      Fpath.(normalize (parent path // actual_rel_path))
  | _ -> path

let _get_compiler_executable_path () =
  let path = _get_argv_0 () in
  let resolved_path = _follow_symlink path in
  Logs.info (fun m ->
      m "Located compiler executable: %s, resolved to %s" (Fpath.to_string path)
        (Fpath.to_string resolved_path));
  _assert_file_exists resolved_path "Herbc compiler executable";
  resolved_path

let _get_runtime_obj_path compiler_path =
  let path = Fpath.(parent compiler_path / runtime_obj_file_name) in
  _assert_file_exists path "Herb Runtime object file 'runtime.o'";
  path

let make_ll_env () =
  let compiler_path = _get_compiler_executable_path () in
  let env =
    {
      compiler_executable_path = compiler_path;
      runtime_obj_path = _get_runtime_obj_path compiler_path;
    }
  in
  Logs.debug (fun m -> m "Initialized ll_environment %s" (show_ll_environment env));
  env