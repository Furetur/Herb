  $ herbc ../../../../../test/parser/000_empty.herb
  { Parsetree.imports = []; decls = [] }
  $ herbc ../../../../../test/parser/001_empty_entry.herb
  { Parsetree.imports = []; decls = [(Parsetree.PEntry [])] }
  $ herbc ../../../../../test/parser/002_simple_arithmetics.herb
  { Parsetree.imports = [];
    decls =
    [(Parsetree.PEntry
        [(Parsetree.PBinOp ((Parsetree.PInt 1), Parsetree.PPlus,
            (Parsetree.PBinOp ((Parsetree.PInt 2), Parsetree.PMul,
               (Parsetree.PInt 3)))
            ))
          ])
      ]
    }
  $ herbc ../../../../../test/parser/003_complex_arithmetics.herb
  { Parsetree.imports = [];
    decls =
    [(Parsetree.PEntry
        [(Parsetree.PBinOp (
            (Parsetree.PBinOp (
               (Parsetree.PBinOp ((Parsetree.PInt 1), Parsetree.PPlus,
                  (Parsetree.PBinOp ((Parsetree.PInt 2), Parsetree.PMul,
                     (Parsetree.PInt 3)))
                  )),
               Parsetree.PPlus,
               (Parsetree.PBinOp (
                  (Parsetree.PBinOp (
                     (Parsetree.PBinOp ((Parsetree.PInt 8), Parsetree.PPlus,
                        (Parsetree.PInt 9))),
                     Parsetree.PDiv, (Parsetree.PInt 16))),
                  Parsetree.PMod, (Parsetree.PInt 4)))
               )),
            Parsetree.PMinus, (Parsetree.PInt 5)))
          ])
      ]
    }
  $ herbc ../../../../../test/parser/004_assign_associativity.herb
  { Parsetree.imports = [];
    decls =
    [(Parsetree.PEntry
        [(Parsetree.PAssign ((Parsetree.PIdent "x"),
            (Parsetree.PAssign ((Parsetree.PIdent "y"),
               (Parsetree.PBinOp ((Parsetree.PIdent "z"), Parsetree.PPlus,
                  (Parsetree.PIdent "w")))
               ))
            ))
          ])
      ]
    }
  $ herbc ../../../../../test/parser/005_toplevel_let.herb
  { Parsetree.imports = [];
    decls = [(Parsetree.PToplevelLet ("f", (Parsetree.PInt 14)))] }
  $ herbc ../../../../../test/parser/006_if_expr.herb
  { Parsetree.imports = [];
    decls =
    [(Parsetree.PToplevelLet ("a", (Parsetree.PInt 2)));
      (Parsetree.PToplevelLet
         ("b",
          Parsetree.PIf {
            cond =
            (Parsetree.PBinOp (
               (Parsetree.PBinOp ((Parsetree.PIdent "a"), Parsetree.PMod,
                  (Parsetree.PInt 2))),
               Parsetree.PEq, (Parsetree.PInt 0)));
            then_ = [(Parsetree.PIdent "true")];
            else_ = [(Parsetree.PIdent "false")]}));
      (Parsetree.PEntry
         [Parsetree.PIf {cond = (Parsetree.PIdent "b");
            then_ = [(Parsetree.PInt 0)]; else_ = [(Parsetree.PInt 1)]}
           ])
      ]
    }
  $ herbc ../../../../../test/parser/007_fun_call.herb
  { Parsetree.imports = [];
    decls =
    [(Parsetree.PToplevelLet
        ("x",
         Parsetree.PFunCall {callee = (Parsetree.PIdent "f");
           args = [(Parsetree.PInt 1)]}));
      (Parsetree.PEntry
         [Parsetree.PFunCall {callee = (Parsetree.PIdent "g");
            args = [(Parsetree.PInt 15)]}
           ])
      ]
    }
  $ herbc ../../../../../test/parser/008_fun_literal.herb
  { Parsetree.imports = [];
    decls =
    [(Parsetree.PToplevelLet
        ("f",
         (Parsetree.PFunLiteral
            { Parsetree.formal_args = [("x", (Parsetree.PTypNamed "int"))];
              body = (Parsetree.PIdent "x") })));
      (Parsetree.PEntry
         [Parsetree.PFunCall {
            callee =
            (Parsetree.PFunLiteral
               { Parsetree.formal_args = [("y", (Parsetree.PTypNamed "int"))];
                 body = (Parsetree.PIdent "y") });
            args = [(Parsetree.PInt 1)]}
           ])
      ]
    }
  $ herbc ../../../../../test/parser/009_local_let.herb
  { Parsetree.imports = [];
    decls =
    [(Parsetree.PEntry
        [(Parsetree.PLet ("a", (Parsetree.PInt 1)));
          (Parsetree.PLet ("b", (Parsetree.PLet ("c", (Parsetree.PInt 2)))));
          (Parsetree.PIdent "a")])
      ]
    }
  $ herbc ../../../../../test/parser/010_fact.herb
  { Parsetree.imports = [];
    decls =
    [(Parsetree.PToplevelLet
        ("fact",
         (Parsetree.PFunLiteral
            { Parsetree.formal_args = [("x", (Parsetree.PTypNamed "int"))];
              body =
              Parsetree.PIf {
                cond =
                (Parsetree.PBinOp ((Parsetree.PIdent "x"), Parsetree.PEq,
                   (Parsetree.PInt 0)));
                then_ = [(Parsetree.PInt 1)];
                else_ =
                [(Parsetree.PBinOp ((Parsetree.PIdent "x"), Parsetree.PMul,
                    Parsetree.PFunCall {callee = (Parsetree.PIdent "fact");
                      args =
                      [(Parsetree.PBinOp ((Parsetree.PIdent "x"),
                          Parsetree.PMinus, (Parsetree.PInt 1)))
                        ]}
                    ))
                  ]}
              })));
      (Parsetree.PEntry
         [Parsetree.PFunCall {callee = (Parsetree.PIdent "print");
            args =
            [Parsetree.PFunCall {callee = (Parsetree.PIdent "fact");
               args = [(Parsetree.PInt 10)]}
              ]}
           ])
      ]
    }
  $ herbc ../../../../../test/parser/011_imports.herb
  { Parsetree.imports =
    [{ Parsetree.repo = (Some "herb"); path = ["containers"; "map"] };
      { Parsetree.repo = None; path = ["utils"] }];
    decls = [(Parsetree.PEntry [])] }

