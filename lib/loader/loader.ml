open Base
open Ast

let ( let* ) = Result.( >>= )

let get_schedule tbl entry =
  let open Ast in
  let get_imported_cus (imps : resolved_import list) =
    List.map imps ~f:(fun { cu; _ } -> cu)
  in
  let get_imports cu =
    Option.(
      value_or_thunk
        (Map.find tbl cu >>| fun m -> get_imported_cus m.Fs_loader.imports)
        ~default:(fun () -> []))
  in
  Tsort.tsort get_imports entry

let check_modules (libs, entry) entry_cu =
  let rec check_libs acc = function
    | h :: libs ->
        let* h = Module_checker.process_lib_file h in
        check_libs (h :: acc) libs
    | [] -> Ok acc
  in
  let* entry = Module_checker.process_entry_file entry_cu entry in
  let* libs = check_libs [] libs in
  Ok { lib_modules = libs; entry_module = entry }

let load_project proj =
  let* tbl = Fs_loader.load_project proj in
  let* libs, entry = get_schedule tbl proj.entry in
  let libs = List.map libs ~f:(Map.find_exn tbl) in
  let entry = Map.find_exn tbl entry in
  check_modules (libs, entry) proj.entry
