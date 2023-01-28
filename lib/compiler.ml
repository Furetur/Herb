let process chan =
  match Parsing_.parse chan with
  | Ok x -> print_endline (Parsing_.Parsetree.show_prog x)
  | Error x -> print_endline x

