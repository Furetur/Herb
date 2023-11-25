open Base
open Stdio
module M = Ollvm.Ez.Module
module P = Ollvm.Printer

let write m formatter =
  let open M in
  P.modul (P.empty_env ()) formatter m.m_module

let write_to_stdout m = write m Stdlib.Format.std_formatter

let write_to_file m filepath: (unit, string) Result.t =
  try
    Out_channel.with_file (Fpath.to_string filepath) ~f:(fun chan ->
        let fmt = Stdlib.Format.formatter_of_out_channel chan in
        write m fmt;
        Ok ())
  with Sys_error msg ->
    Error (Printf.sprintf "Failed to write to file: %s" msg)
