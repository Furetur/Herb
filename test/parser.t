  $ parse ../../../../../test/parser/000_empty.herb
  { Ast.imports = []; decls = [] }
  $ parse ../../../../../test/parser/001_empty_entry.herb
  { Ast.imports = []; decls = [(Ast.AEntry [])] }
  $ parse ../../../../../test/parser/002_simple_arithmetics.herb
  { Ast.imports = [];
    decls =
    [(Ast.AEntry
        [(Ast.AExprStmt
            (Ast.ABinOp ((Ast.ALiteral (Ast.AInt 1)), Ast.APlus,
               (Ast.ABinOp ((Ast.ALiteral (Ast.AInt 2)), Ast.AMul,
                  (Ast.ALiteral (Ast.AInt 3))))
               )))
          ])
      ]
    }
  $ parse ../../../../../test/parser/003_complex_arithmetics.herb
  { Ast.imports = [];
    decls =
    [(Ast.AEntry
        [(Ast.AExprStmt
            (Ast.ABinOp (
               (Ast.ABinOp (
                  (Ast.ABinOp ((Ast.ALiteral (Ast.AInt 1)), Ast.APlus,
                     (Ast.ABinOp ((Ast.ALiteral (Ast.AInt 2)), Ast.AMul,
                        (Ast.ALiteral (Ast.AInt 3))))
                     )),
                  Ast.APlus,
                  (Ast.ABinOp (
                     (Ast.ABinOp (
                        (Ast.ABinOp ((Ast.ALiteral (Ast.AInt 8)), Ast.APlus,
                           (Ast.ALiteral (Ast.AInt 9)))),
                        Ast.ADiv, (Ast.ALiteral (Ast.AInt 16)))),
                     Ast.AMod, (Ast.ALiteral (Ast.AInt 4))))
                  )),
               Ast.AMinus, (Ast.ALiteral (Ast.AInt 5)))))
          ])
      ]
    }
  $ parse ../../../../../test/parser/004_assign_associativity.herb
  { Ast.imports = [];
    decls =
    [(Ast.AEntry
        [(Ast.AExprStmt
            (Ast.AAssign ((Ast.AIdent "x"),
               (Ast.AAssign ((Ast.AIdent "y"),
                  (Ast.ABinOp ((Ast.AIdent "z"), Ast.APlus, (Ast.AIdent "w")))
                  ))
               )))
          ])
      ]
    }
  $ parse ../../../../../test/parser/005_toplevel_let.herb
  { Ast.imports = [];
    decls = [(Ast.AToplevelLet ("f", (Ast.ALiteral (Ast.AInt 14))))] }
  $ parse ../../../../../test/parser/006_if_expr.herb
  { Ast.imports = [];
    decls =
    [(Ast.AToplevelLet ("a", (Ast.ALiteral (Ast.AInt 2))));
      (Ast.AToplevelLet
         ("b",
          Ast.AIf {
            cond =
            (Ast.ABinOp (
               (Ast.ABinOp ((Ast.AIdent "a"), Ast.AMod,
                  (Ast.ALiteral (Ast.AInt 2)))),
               Ast.AEq, (Ast.ALiteral (Ast.AInt 0))));
            then_ = [(Ast.AExprStmt (Ast.AIdent "true"))];
            else_ = [(Ast.AExprStmt (Ast.AIdent "false"))]}));
      (Ast.AEntry
         [(Ast.AExprStmt
             Ast.AIf {cond = (Ast.AIdent "b");
               then_ = [(Ast.AExprStmt (Ast.ALiteral (Ast.AInt 0)))];
               else_ = [(Ast.AExprStmt (Ast.ALiteral (Ast.AInt 1)))]})
           ])
      ]
    }
  $ parse ../../../../../test/parser/007_fun_call.herb
  { Ast.imports = [];
    decls =
    [(Ast.AToplevelLet
        ("x",
         Ast.AFunCall {callee = (Ast.AIdent "f");
           args = [(Ast.ALiteral (Ast.AInt 1))]}));
      (Ast.AEntry
         [(Ast.AExprStmt
             Ast.AFunCall {callee = (Ast.AIdent "g");
               args = [(Ast.ALiteral (Ast.AInt 15))]})
           ])
      ]
    }
  $ parse ../../../../../test/parser/008_fun_literal.herb
  { Ast.imports = [];
    decls =
    [(Ast.AToplevelLet
        ("f",
         (Ast.ALiteral
            Ast.AFun {fargs = [("x", (Ast.ATypNamed "int"))];
              body = (Ast.AIdent "x")})));
      (Ast.AEntry
         [(Ast.AExprStmt
             Ast.AFunCall {
               callee =
               (Ast.ALiteral
                  Ast.AFun {fargs = [("y", (Ast.ATypNamed "int"))];
                    body = (Ast.AIdent "y")});
               args = [(Ast.ALiteral (Ast.AInt 1))]})
           ])
      ]
    }
  $ parse ../../../../../test/parser/009_local_let.herb
  Syntax error at line 3 column 12
  $ parse ../../../../../test/parser/010_fact.herb
  { Ast.imports = [];
    decls =
    [(Ast.AToplevelLet
        ("fact",
         (Ast.ALiteral
            Ast.AFun {fargs = [("x", (Ast.ATypNamed "int"))];
              body =
              Ast.AIf {
                cond =
                (Ast.ABinOp ((Ast.AIdent "x"), Ast.AEq,
                   (Ast.ALiteral (Ast.AInt 0))));
                then_ = [(Ast.AExprStmt (Ast.ALiteral (Ast.AInt 1)))];
                else_ =
                [(Ast.AExprStmt
                    (Ast.ABinOp ((Ast.AIdent "x"), Ast.AMul,
                       Ast.AFunCall {callee = (Ast.AIdent "fact");
                         args =
                         [(Ast.ABinOp ((Ast.AIdent "x"), Ast.AMinus,
                             (Ast.ALiteral (Ast.AInt 1))))
                           ]}
                       )))
                  ]}})));
      (Ast.AEntry
         [(Ast.AExprStmt
             Ast.AFunCall {callee = (Ast.AIdent "print");
               args =
               [Ast.AFunCall {callee = (Ast.AIdent "fact");
                  args = [(Ast.ALiteral (Ast.AInt 10))]}
                 ]})
           ])
      ]
    }
  $ parse ../../../../../test/parser/011_imports.herb
  { Ast.imports =
    [{ Ast.herbarium = (Some "herb"); path = ["containers"; "map"] };
      { Ast.herbarium = None; path = ["utils"] }];
    decls = [(Ast.AEntry [])] }
  $ parse ../../../../../test/parser/012_string_literal.herb
  { Ast.imports = [];
    decls =
    [(Ast.AToplevelLet ("a", (Ast.ALiteral (Ast.AString "123"))));
      (Ast.AToplevelLet
         ("b",
          (Ast.ALiteral
             (Ast.AString "\208\159\209\128\208\184\208\178\208\181\209\130"))));
      (Ast.AEntry
         [(Ast.AExprStmt
             (Ast.ALiteral (Ast.AString "\208\159\208\190\208\186\208\176!bye")))
           ])
      ]
    }
  $ parse ../../../../../test/parser/013_for_numeric.herb
  { Ast.imports = [];
    decls =
    [(Ast.AToplevelLet
        ("x",
         (Ast.ABlock
            [(Ast.ALetStmt ("i", (Ast.ALiteral (Ast.AInt 1))));
              (Ast.AExprStmt
                 Ast.AWhile {
                   cond =
                   (Ast.ABinOp ((Ast.AIdent "i"), Ast.ALte,
                      (Ast.ALiteral (Ast.AInt 10))));
                   body =
                   [(Ast.AExprStmt
                       (Ast.ABlock
                          [(Ast.AExprStmt
                              Ast.AFunCall {callee = (Ast.AIdent "print");
                                args = [(Ast.AIdent "i")]})
                            ]));
                     (Ast.AExprStmt
                        (Ast.AAssign ((Ast.AIdent "i"),
                           (Ast.ABinOp ((Ast.AIdent "i"), Ast.APlus,
                              (Ast.ALiteral (Ast.AInt 1))))
                           )))
                     ]})
              ])));
      (Ast.AEntry
         [(Ast.AExprStmt
             (Ast.ABlock
                [(Ast.ALetStmt ("j", (Ast.ALiteral (Ast.AInt 1))));
                  (Ast.AExprStmt
                     Ast.AWhile {
                       cond =
                       (Ast.ABinOp ((Ast.AIdent "j"), Ast.ALte,
                          (Ast.ALiteral (Ast.AInt 100))));
                       body =
                       [(Ast.AExprStmt
                           (Ast.ABlock
                              [(Ast.AExprStmt
                                  Ast.AFunCall {callee = (Ast.AIdent "call");
                                    args = [(Ast.AIdent "j")]})
                                ]));
                         (Ast.AExprStmt
                            (Ast.AAssign ((Ast.AIdent "j"),
                               (Ast.ABinOp ((Ast.AIdent "j"), Ast.APlus,
                                  (Ast.ALiteral (Ast.AInt 1))))
                               )))
                         ]})
                  ]))
           ])
      ]
    }
  $ parse ../../../../../test/parser/014_extern.herb
  { Ast.imports = [];
    decls =
    [(Ast.AExtern
        { Ast.name = "i"; typ = (Ast.ATypNamed "int"); linkname = "counter" })
      ]
    }
  $ parse ../../../../../test/parser/015_extern_fun.herb
  { Ast.imports = [];
    decls =
    [(Ast.AExtern
        { Ast.name = "printint";
          typ =
          Ast.ATypFun {farg_types = [(Ast.ATypNamed "int")];
            ret_typ = (Ast.ATypNamed "unit")};
          linkname = "__print_int_" });
      (Ast.AExtern
         { Ast.name = "sprintf";
           typ =
           Ast.ATypFun {
             farg_types =
             [(Ast.ATypNamed "string"); (Ast.ATypNamed "int");
               (Ast.ATypNamed "int")];
             ret_typ = (Ast.ATypNamed "string")};
           linkname = "sprintf" })
      ]
    }
  $ parse ../../../../../test/parser/016_while.herb
  { Ast.imports = [];
    decls =
    [(Ast.AToplevelLet
        ("f",
         Ast.AWhile {cond = (Ast.AIdent "x");
           body =
           [(Ast.AExprStmt
               Ast.AFunCall {callee = (Ast.AIdent "print");
                 args = [(Ast.AIdent "x")]})
             ]}));
      (Ast.AEntry
         [(Ast.ALetStmt ("i", (Ast.ALiteral (Ast.AInt 0))));
           (Ast.AExprStmt
              Ast.AWhile {
                cond =
                (Ast.ABinOp ((Ast.AIdent "i"), Ast.ALt,
                   (Ast.ALiteral (Ast.AInt 100))));
                body =
                [(Ast.AExprStmt
                    Ast.AFunCall {callee = (Ast.AIdent "print");
                      args = [(Ast.AIdent "i")]});
                  (Ast.AExprStmt
                     (Ast.AAssign ((Ast.AIdent "i"),
                        (Ast.ABinOp ((Ast.AIdent "i"), Ast.APlus,
                           (Ast.ALiteral (Ast.AInt 1))))
                        )))
                  ]})
           ])
      ]
    }
