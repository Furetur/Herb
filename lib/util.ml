open Base

let pushback list el = List.append list [ el ]

let parse_path path =
  match Fpath.of_string path with
  | Error (`Msg s) -> Error [ Err_templates.invalid_path_error s ]
  | Ok path -> Ok path
