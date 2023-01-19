let () =
  let c = open_in "example.herb" in
  Herb.Compiler.process c;

