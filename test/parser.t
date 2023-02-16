  $ parse ../../../../../test/parser/000_empty.herb
  { Parsetree.imports = []; decls = [] }
  $ parse ../../../../../test/parser/001_empty_entry.herb
  { Parsetree.imports = []; decls = [(Parsetree.PEntry [])] }
  $ parse ../../../../../test/parser/002_simple_arithmetics.herb
  { Parsetree.imports = [];
    decls =
    [(Parsetree.PEntry
        [(Parsetree.PExprStmt
            (Parsetree.PBinOp ((Parsetree.PInt 1), Parsetree.PPlus,
               (Parsetree.PBinOp ((Parsetree.PInt 2), Parsetree.PMul,
                  (Parsetree.PInt 3)))
               )))
          ])
      ]
    }
  $ parse ../../../../../test/parser/003_complex_arithmetics.herb
  { Parsetree.imports = [];
    decls =
    [(Parsetree.PEntry
        [(Parsetree.PExprStmt
            (Parsetree.PBinOp (
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
               Parsetree.PMinus, (Parsetree.PInt 5))))
          ])
      ]
    }
  $ parse ../../../../../test/parser/004_assign_associativity.herb
  { Parsetree.imports = [];
    decls =
    [(Parsetree.PEntry
        [(Parsetree.PExprStmt
            (Parsetree.PAssign ((Parsetree.PIdent "x"),
               (Parsetree.PAssign ((Parsetree.PIdent "y"),
                  (Parsetree.PBinOp ((Parsetree.PIdent "z"), Parsetree.PPlus,
                     (Parsetree.PIdent "w")))
                  ))
               )))
          ])
      ]
    }
  $ parse ../../../../../test/parser/005_toplevel_let.herb
  { Parsetree.imports = [];
    decls = [(Parsetree.PToplevelLet ("f", (Parsetree.PInt 14)))] }
  $ parse ../../../../../test/parser/006_if_expr.herb
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
            then_ = [(Parsetree.PExprStmt (Parsetree.PIdent "true"))];
            else_ = [(Parsetree.PExprStmt (Parsetree.PIdent "false"))]}));
      (Parsetree.PEntry
         [(Parsetree.PExprStmt
             Parsetree.PIf {cond = (Parsetree.PIdent "b");
               then_ = [(Parsetree.PExprStmt (Parsetree.PInt 0))];
               else_ = [(Parsetree.PExprStmt (Parsetree.PInt 1))]})
           ])
      ]
    }
  $ parse ../../../../../test/parser/007_fun_call.herb
  { Parsetree.imports = [];
    decls =
    [(Parsetree.PToplevelLet
        ("x",
         Parsetree.PFunCall {callee = (Parsetree.PIdent "f");
           args = [(Parsetree.PInt 1)]}));
      (Parsetree.PEntry
         [(Parsetree.PExprStmt
             Parsetree.PFunCall {callee = (Parsetree.PIdent "g");
               args = [(Parsetree.PInt 15)]})
           ])
      ]
    }
  $ parse ../../../../../test/parser/008_fun_literal.herb
  { Parsetree.imports = [];
    decls =
    [(Parsetree.PToplevelLet
        ("f",
         (Parsetree.PFunLiteral
            { Parsetree.formal_args = [("x", (Parsetree.PTypNamed "int"))];
              body = (Parsetree.PIdent "x") })));
      (Parsetree.PEntry
         [(Parsetree.PExprStmt
             Parsetree.PFunCall {
               callee =
               (Parsetree.PFunLiteral
                  { Parsetree.formal_args =
                    [("y", (Parsetree.PTypNamed "int"))];
                    body = (Parsetree.PIdent "y") });
               args = [(Parsetree.PInt 1)]})
           ])
      ]
    }
  $ parse ../../../../../test/parser/009_local_let.herb
  Syntax error at line 3 column 12
  $ parse ../../../../../test/parser/010_fact.herb
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
                then_ = [(Parsetree.PExprStmt (Parsetree.PInt 1))];
                else_ =
                [(Parsetree.PExprStmt
                    (Parsetree.PBinOp ((Parsetree.PIdent "x"), Parsetree.PMul,
                       Parsetree.PFunCall {callee = (Parsetree.PIdent "fact");
                         args =
                         [(Parsetree.PBinOp ((Parsetree.PIdent "x"),
                             Parsetree.PMinus, (Parsetree.PInt 1)))
                           ]}
                       )))
                  ]}
              })));
      (Parsetree.PEntry
         [(Parsetree.PExprStmt
             Parsetree.PFunCall {callee = (Parsetree.PIdent "print");
               args =
               [Parsetree.PFunCall {callee = (Parsetree.PIdent "fact");
                  args = [(Parsetree.PInt 10)]}
                 ]})
           ])
      ]
    }
  $ parse ../../../../../test/parser/011_imports.herb
  { Parsetree.imports =
    [{ Parsetree.herbarium = (Some "herb"); path = ["containers"; "map"] };
      { Parsetree.herbarium = None; path = ["utils"] }];
    decls = [(Parsetree.PEntry [])] }
  $ parse ../../../../../test/parser/012_string_literal.herb
  { Parsetree.imports = [];
    decls =
    [(Parsetree.PToplevelLet ("a", (Parsetree.PString "123")));
      (Parsetree.PToplevelLet
         ("b",
          (Parsetree.PString "\208\159\209\128\208\184\208\178\208\181\209\130")));
      (Parsetree.PEntry
         [(Parsetree.PExprStmt
             (Parsetree.PString "\208\159\208\190\208\186\208\176!bye"))
           ])
      ]
    }
  $ parse ../../../../../test/parser/013_for_numeric.herb
  { Parsetree.imports = [];
    decls =
    [(Parsetree.PToplevelLet
        ("x",
         Parsetree.PFor {i = "i"; start_ = (Parsetree.PInt 1);
           end_ = (Parsetree.PInt 10);
           body =
           [(Parsetree.PExprStmt
               Parsetree.PFunCall {callee = (Parsetree.PIdent "print");
                 args = [(Parsetree.PIdent "i")]})
             ]}));
      (Parsetree.PEntry
         [(Parsetree.PExprStmt
             Parsetree.PFor {i = "j"; start_ = (Parsetree.PInt 1);
               end_ = (Parsetree.PInt 100);
               body =
               [(Parsetree.PExprStmt
                   Parsetree.PFunCall {callee = (Parsetree.PIdent "call");
                     args = [(Parsetree.PIdent "j")]})
                 ]})
           ])
      ]
    }
  $ parse ../../../../../test/parser/014_extern.herb
  { Parsetree.imports = [];
    decls =
    [Parsetree.PExtern {name = "i"; typ = (Parsetree.PTypNamed "int");
       linkname = "counter"}
      ]
    }
  $ parse ../../../../../test/parser/015_extern_fun.herb
  { Parsetree.imports = [];
    decls =
    [Parsetree.PExtern {name = "printint";
       typ =
       (Parsetree.PTypFun
          { Parsetree.arg_types = [(Parsetree.PTypNamed "int")];
            ret_typ = (Parsetree.PTypNamed "unit") });
       linkname = "__print_int_"};
      Parsetree.PExtern {name = "sprintf";
        typ =
        (Parsetree.PTypFun
           { Parsetree.arg_types =
             [(Parsetree.PTypNamed "string"); (Parsetree.PTypNamed "int");
               (Parsetree.PTypNamed "int")];
             ret_typ = (Parsetree.PTypNamed "string") });
        linkname = "sprintf"}
      ]
    }
  $ parse ../../../../../test/parser/016_while.herb
  { Parsetree.imports = [];
    decls =
    [(Parsetree.PToplevelLet
        ("f",
         Parsetree.PWhile {cond = (Parsetree.PIdent "x");
           body =
           [(Parsetree.PExprStmt
               Parsetree.PFunCall {callee = (Parsetree.PIdent "print");
                 args = [(Parsetree.PIdent "x")]})
             ]}));
      (Parsetree.PEntry
         [(Parsetree.PLet ("i", (Parsetree.PInt 0)));
           (Parsetree.PExprStmt
              Parsetree.PWhile {
                cond =
                (Parsetree.PBinOp ((Parsetree.PIdent "i"), Parsetree.PLt,
                   (Parsetree.PInt 100)));
                body =
                [(Parsetree.PExprStmt
                    Parsetree.PFunCall {callee = (Parsetree.PIdent "print");
                      args = [(Parsetree.PIdent "i")]});
                  (Parsetree.PExprStmt
                     (Parsetree.PAssign ((Parsetree.PIdent "i"),
                        (Parsetree.PBinOp ((Parsetree.PIdent "i"),
                           Parsetree.PPlus, (Parsetree.PInt 1)))
                        )))
                  ]})
           ])
      ]
    }
