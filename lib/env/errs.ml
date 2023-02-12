open Base

type err_kind = ImportError | SyntaxError

type err = {
  cu : Proj.cu;
  loc : Loc.t;
  kind : err_kind;
  title : string;
  text : string;
}

let show_kind = function
  | ImportError -> "ImportError"
  | SyntaxError -> "SyntaxError"

let show_err { cu; loc; kind; title; text } =
  let path = Fpath.to_string (Proj.cu_path cu) in
  let line, col = Loc.start_line_col loc in
  let err =
    Printf.sprintf "File '%s', line %d, column %d:\n[%s] %s" path line col
      (show_kind kind) title
  in
  if String.(text = "") then err else err ^ "\n" ^ text

let show_errs errs = String.concat ~sep:"\n\n" (List.map errs ~f:show_err)
