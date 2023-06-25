  $ herbc -p ../../../../../test/parser/000_empty.herb
  { Ast.decls = [] }
  $ herbc -p ../../../../../test/parser/001_empty_entry.herb
  { Ast.decls = [(Ast.AEntry [])] }
  $ herbc -p ../../../../../test/parser/002_simple_arithmetics.herb
  { Ast.decls =
    [(Ast.AEntry
        [(Ast.AExprStmt
            (Ast.ABinOp ((Ast.ALiteral (Ast.AInt 1)), Ast_operators.APlus,
               (Ast.ABinOp ((Ast.ALiteral (Ast.AInt 2)), Ast_operators.AMul,
                  (Ast.ALiteral (Ast.AInt 3))))
               )))
          ])
      ]
    }
  $ herbc -p ../../../../../test/parser/003_complex_arithmetics.herb
  { Ast.decls =
    [(Ast.AEntry
        [(Ast.AExprStmt
            (Ast.ABinOp (
               (Ast.ABinOp (
                  (Ast.ABinOp ((Ast.ALiteral (Ast.AInt 1)),
                     Ast_operators.APlus,
                     (Ast.ABinOp ((Ast.ALiteral (Ast.AInt 2)),
                        Ast_operators.AMul, (Ast.ALiteral (Ast.AInt 3))))
                     )),
                  Ast_operators.APlus,
                  (Ast.ABinOp (
                     (Ast.ABinOp (
                        (Ast.ABinOp ((Ast.ALiteral (Ast.AInt 8)),
                           Ast_operators.APlus, (Ast.ALiteral (Ast.AInt 9)))),
                        Ast_operators.ADiv, (Ast.ALiteral (Ast.AInt 16)))),
                     Ast_operators.AMod, (Ast.ALiteral (Ast.AInt 4))))
                  )),
               Ast_operators.AMinus, (Ast.ALiteral (Ast.AInt 5)))))
          ])
      ]
    }
  $ herbc -p ../../../../../test/parser/004_assign_associativity.herb
  { Ast.decls =
    [(Ast.AEntry
        [(Ast.AExprStmt
            (Ast.AAssign ((Ast.AIdent "x"),
               (Ast.AAssign ((Ast.AIdent "y"),
                  (Ast.ABinOp ((Ast.AIdent "z"), Ast_operators.APlus,
                     (Ast.AIdent "w")))
                  ))
               )))
          ])
      ]
    }
  $ herbc -p ../../../../../test/parser/005_toplevel_let.herb
  { Ast.decls = [(Ast.AToplevelLet ("f", (Ast.ALiteral (Ast.AInt 14))))] }
  $ herbc -p ../../../../../test/parser/006_if_expr.herb
  { Ast.decls =
    [(Ast.AToplevelLet ("a", (Ast.ALiteral (Ast.AInt 2))));
      (Ast.AToplevelLet
         ("b",
          Ast.AIf {
            cond =
            (Ast.ABinOp (
               (Ast.ABinOp ((Ast.AIdent "a"), Ast_operators.AMod,
                  (Ast.ALiteral (Ast.AInt 2)))),
               Ast_operators.AEq, (Ast.ALiteral (Ast.AInt 0))));
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
  $ herbc -p ../../../../../test/parser/007_fun_call.herb
  { Ast.decls =
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
  $ herbc -p ../../../../../test/parser/008_fun_literal.herb
  { Ast.decls =
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
  $ herbc -p ../../../../../test/parser/009_local_let.herb
  File '../../../../../test/parser/009_local_let.herb', line 3, column 12:
  Error: Syntax error
  $ herbc -p ../../../../../test/parser/010_fact.herb
  { Ast.decls =
    [(Ast.AToplevelLet
        ("fact",
         (Ast.ALiteral
            Ast.AFun {fargs = [("x", (Ast.ATypNamed "int"))];
              body =
              Ast.AIf {
                cond =
                (Ast.ABinOp ((Ast.AIdent "x"), Ast_operators.AEq,
                   (Ast.ALiteral (Ast.AInt 0))));
                then_ = [(Ast.AExprStmt (Ast.ALiteral (Ast.AInt 1)))];
                else_ =
                [(Ast.AExprStmt
                    (Ast.ABinOp ((Ast.AIdent "x"), Ast_operators.AMul,
                       Ast.AFunCall {callee = (Ast.AIdent "fact");
                         args =
                         [(Ast.ABinOp ((Ast.AIdent "x"), Ast_operators.AMinus,
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
  $ herbc -p ../../../../../test/parser/011_imports.herb
  File '../../../../../test/parser/011_imports.herb', line 1, column 0:
  Error: Syntax error
  $ herbc -p ../../../../../test/parser/012_string_literal.herb
  { Ast.decls =
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
  $ herbc -p ../../../../../test/parser/013_for_numeric.herb
  File '../../../../../test/parser/013_for_numeric.herb', line 1, column 8:
  Error: Syntax error
  $ herbc -p ../../../../../test/parser/014_extern.herb
  { Ast.decls =
    [(Ast.AExtern
        { Ast.name = "i"; typ = (Ast.ATypNamed "int"); linkname = "counter" });
      (Ast.AEntry [])]
    }
  $ herbc -p ../../../../../test/parser/015_extern_fun.herb
  { Ast.decls =
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
           linkname = "sprintf" });
      (Ast.AEntry [])]
    }
  $ herbc -p ../../../../../test/parser/016_while.herb
  { Ast.decls =
    [(Ast.AExtern
        { Ast.name = "print";
          typ =
          Ast.ATypFun {farg_types = [(Ast.ATypNamed "int")];
            ret_typ = (Ast.ATypNamed "unit")};
          linkname = "__print" });
      (Ast.AToplevelLet ("x", (Ast.ALiteral (Ast.AInt 0))));
      (Ast.AToplevelLet
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
                (Ast.ABinOp ((Ast.AIdent "i"), Ast_operators.ALt,
                   (Ast.ALiteral (Ast.AInt 100))));
                body =
                [(Ast.AExprStmt
                    Ast.AFunCall {callee = (Ast.AIdent "print");
                      args = [(Ast.AIdent "i")]});
                  (Ast.AExprStmt
                     (Ast.AAssign ((Ast.AIdent "i"),
                        (Ast.ABinOp ((Ast.AIdent "i"), Ast_operators.APlus,
                           (Ast.ALiteral (Ast.AInt 1))))
                        )))
                  ]})
           ])
      ]
    }
