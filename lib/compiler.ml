open Base
open Stdio

let compile path =
  match Loader.load_project path with
  | Ok x -> print_endline (Parsetree.show_herbfile x)
  | Error x -> print_endline x
