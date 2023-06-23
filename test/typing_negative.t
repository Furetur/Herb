  $ herbc ../../../../../test/typing_negative/000_assignment.herb
  File '../../../../../test/typing_negative/000_assignment.herb', line 3, column 9:
  Error: Type error
  Expected 'int' but got 'string'
  
  File '../../../../../test/typing_negative/000_assignment.herb', line 4, column 9:
  Error: Type error
  Expected 'int' but got '(int) -> string'
  $ herbc ../../../../../test/typing_negative/001_unit.herb
  File '../../../../../test/typing_negative/001_unit.herb', line 4, column 9:
  Error: Type error
  Expected 'unit' but got 'int'
  $ herbc ../../../../../test/typing_negative/002_arguments.herb
  File '../../../../../test/typing_negative/002_arguments.herb', line 5, column 9:
  Error: Type error
  Expected 'string' but got 'int'
  
  File '../../../../../test/typing_negative/002_arguments.herb', line 6, column 6:
  Error: Type error
  Expected 'int' but got 'unit'
  
  File '../../../../../test/typing_negative/002_arguments.herb', line 6, column 12:
  Error: Type error
  Expected 'string' but got 'unit'
  
  File '../../../../../test/typing_negative/002_arguments.herb', line 7, column 6:
  Error: Type error
  Expected 'int' but got 'string'
  $ herbc ../../../../../test/typing_negative/003_return_type_inference.herb
  File '../../../../../test/typing_negative/003_return_type_inference.herb', line 5, column 9:
  Error: Type error
  Expected 'int' but got 'string'
  $ herbc ../../../../../test/typing_negative/004_return_type_inference_2.herb
  File '../../../../../test/typing_negative/004_return_type_inference_2.herb', line 9, column 9:
  Error: Type error
  Expected 'string' but got 'int'
  $ herbc ../../../../../test/typing_negative/005_block_type_inference.herb
  File '../../../../../test/typing_negative/005_block_type_inference.herb', line 3, column 9:
  Error: Type error
  Expected 'int' but got 'string'
  
  File '../../../../../test/typing_negative/005_block_type_inference.herb', line 6, column 9:
  Error: Type error
  Expected 'string' but got 'int'
  
  File '../../../../../test/typing_negative/005_block_type_inference.herb', line 9, column 9:
  Error: Type error
  Expected '(int) -> int' but got '(string) -> string'
  $ herbc ../../../../../test/typing_negative/006_unit_return_type.herb
  File '../../../../../test/typing_negative/006_unit_return_type.herb', line 5, column 9:
  Error: Type error
  Expected 'unit' but got 'int'
  $ herbc ../../../../../test/typing_negative/007_unary_operators.herb
  File '../../../../../test/typing_negative/007_unary_operators.herb', line 2, column 13:
  Error: Type error
  Expected 'bool' but got 'int'
  
  File '../../../../../test/typing_negative/007_unary_operators.herb', line 3, column 13:
  Error: Type error
  Expected 'bool' but got 'string'
  
  File '../../../../../test/typing_negative/007_unary_operators.herb', line 5, column 13:
  Error: Type error
  Expected 'bool' but got 'int'
  $ herbc ../../../../../test/typing_negative/008_binary_operator.herb
  File '../../../../../test/typing_negative/008_binary_operator.herb', line 2, column 4:
  Error: Binary operation between provided types is not defined: 'int + string'
  
  File '../../../../../test/typing_negative/008_binary_operator.herb', line 3, column 4:
  Error: Binary operation between provided types is not defined: 'string + int'
  
  File '../../../../../test/typing_negative/008_binary_operator.herb', line 4, column 4:
  Error: Binary operation between provided types is not defined: 'string * int'
  
  File '../../../../../test/typing_negative/008_binary_operator.herb', line 5, column 4:
  Error: Binary operation between provided types is not defined: 'string * string'
  
  File '../../../../../test/typing_negative/008_binary_operator.herb', line 6, column 4:
  Error: Binary operation between provided types is not defined: 'string - string'
  
  File '../../../../../test/typing_negative/008_binary_operator.herb', line 7, column 4:
  Error: Binary operation between provided types is not defined: 'string / string'
  
  File '../../../../../test/typing_negative/008_binary_operator.herb', line 8, column 4:
  Error: Binary operation between provided types is not defined: 'string % string'
  
  File '../../../../../test/typing_negative/008_binary_operator.herb', line 10, column 4:
  Error: Binary operation between provided types is not defined: 'string < int'
  
  File '../../../../../test/typing_negative/008_binary_operator.herb', line 11, column 4:
  Error: Binary operation between provided types is not defined: 'string < string'
  $ herbc ../../../../../test/typing_negative/009_binary_operator_inference.herb
  herb: internal error, uncaught exception:
        Failure("Type for identifier 'boolean#1.0' not found")
        Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
        Called from Herb__Pass.Pass.let* in file "lib/pass.ml", line 33, characters 15-18
        Called from Herb__Pass.Pass.(<*>) in file "lib/pass.ml", line 42, characters 15-19
        Called from Herb__Pass.Pass.(<*>) in file "lib/pass.ml", line 41, characters 15-19
        Called from Herb__Pass.Pass.let* in file "lib/pass.ml", line 33, characters 15-18
        Called from Herb__Pass.Pass.let* in file "lib/pass.ml", line 33, characters 15-18
        Called from Herb__Pass.Pass.(<*>) in file "lib/pass.ml", line 42, characters 15-19
        Called from Herb__Pass.Pass.let* in file "lib/pass.ml", line 33, characters 15-18
        Called from Herb__Pass.Pass.let* in file "lib/pass.ml", line 33, characters 15-18
        Called from Herb__Pass.Pass.let* in file "lib/pass.ml", line 33, characters 15-18
        Called from Herb__Pass.Pass.run_pass in file "lib/pass.ml", line 13, characters 15-21
        Called from Herb__Compiler.compile in file "lib/compiler.ml", line 28, characters 16-33
        Called from Herb__Compiler.run_compiler in file "lib/compiler.ml", line 33, characters 8-23
        Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 24, characters 19-24
        Called from Cmdliner_eval.run_parser in file "cmdliner_eval.ml", line 34, characters 37-44
  [125]
  $ herbc ../../../../../test/typing_negative/010_true_false.herb
  { Typed_ast.decls = [];
    entry =
    [(Typed_ast.TLetStmt
        (Ident.Local {name = "true"; scope = 1; id = 0},
         { Typ.typ = Typ.Int;
           node =
           (Typed_ast.TBinOp (
              { Typ.typ = Typ.Int;
                node = (Typed_ast.TLiteral (Typed_ast.TInt 1)) },
              Ast_operators.AEq,
              { Typ.typ = Typ.Int;
                node = (Typed_ast.TLiteral (Typed_ast.TInt 1)) }
              ))
           }));
      (Typed_ast.TLetStmt
         (Ident.Local {name = "false"; scope = 1; id = 1},
          { Typ.typ = Typ.Int;
            node =
            (Typed_ast.TBinOp (
               { Typ.typ = Typ.Int;
                 node = (Typed_ast.TLiteral (Typed_ast.TInt 1)) },
               Ast_operators.ANeq,
               { Typ.typ = Typ.Int;
                 node = (Typed_ast.TLiteral (Typed_ast.TInt 1)) }
               ))
            }));
      (Typed_ast.TExprStmt
         { Typ.typ = Typ.Int;
           node =
           (Typed_ast.TAssign (
              { Typ.typ = Typ.Int;
                node =
                (Typed_ast.TIdent
                   Ident.Local {name = "true"; scope = 1; id = 0})
                },
              { Typ.typ = Typ.Int;
                node = (Typed_ast.TLiteral (Typed_ast.TInt 1)) }
              ))
           });
      (Typed_ast.TExprStmt
         { Typ.typ = Typ.Int;
           node =
           (Typed_ast.TAssign (
              { Typ.typ = Typ.Int;
                node =
                (Typed_ast.TIdent
                   Ident.Local {name = "false"; scope = 1; id = 1})
                },
              { Typ.typ = Typ.Int;
                node = (Typed_ast.TLiteral (Typed_ast.TInt 1)) }
              ))
           })
      ]
    }









