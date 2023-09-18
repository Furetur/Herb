  $ herbc ../../../../../test/typing_negative/000_assignment.herb
  File '../../../../../test/typing_negative/000_assignment.herb', line 3, column 9:
  Error: Type error
  Expected 'int' but got 'string'
  [1]
  $ herbc ../../../../../test/typing_negative/001_unit.herb
  File '../../../../../test/typing_negative/001_unit.herb', line 3, column 9:
  Error: Type error
  Expected 'unit' but got 'int'
  [1]
  $ herbc ../../../../../test/typing_negative/002_arguments.herb
  File '../../../../../test/typing_negative/002_arguments.herb', line 4, column 9:
  Error: Type error
  Expected 'string' but got 'int'
  [1]
  $ herbc ../../../../../test/typing_negative/003_return_type_inference.herb
  File '../../../../../test/typing_negative/003_return_type_inference.herb', line 5, column 9:
  Error: Type error
  Expected 'int' but got 'string'
  [1]
  $ herbc ../../../../../test/typing_negative/004_return_type_inference_2.herb
  File '../../../../../test/typing_negative/004_return_type_inference_2.herb', line 9, column 9:
  Error: Type error
  Expected 'string' but got 'int'
  [1]
  $ herbc ../../../../../test/typing_negative/005_block_type_inference.herb
  File '../../../../../test/typing_negative/005_block_type_inference.herb', line 3, column 9:
  Error: Type error
  Expected 'int' but got 'string'
  [1]
  $ herbc ../../../../../test/typing_negative/006_unit_return_type.herb
  File '../../../../../test/typing_negative/006_unit_return_type.herb', line 5, column 9:
  Error: Type error
  Expected 'unit' but got 'int'
  [1]
  $ herbc ../../../../../test/typing_negative/007_unary_operators.herb
  File '../../../../../test/typing_negative/007_unary_operators.herb', line 2, column 13:
  Error: Type error
  Expected 'bool' but got 'int'
  [1]
  $ herbc ../../../../../test/typing_negative/008_binary_operator.herb
  File '../../../../../test/typing_negative/008_binary_operator.herb', line 2, column 4:
  Error: Binary operation between provided types is not defined: 'int + string'
  [1]
  $ herbc ../../../../../test/typing_negative/009_binary_operator_inference.herb
  File '../../../../../test/typing_negative/009_binary_operator_inference.herb', line 3, column 15:
  Error: Type error
  Expected 'bool' but got 'int'
  [1]
  $ herbc ../../../../../test/typing_negative/010_true_false.herb
  File '../../../../../test/typing_negative/010_true_false.herb', line 5, column 12:
  Error: Type error
  Expected 'bool' but got 'int'
  [1]
  $ herbc ../../../../../test/typing_negative/011_assignment_fun.herb
  File '../../../../../test/typing_negative/011_assignment_fun.herb', line 3, column 9:
  Error: Type error
  Expected 'int' but got '(int) -> string'
  [1]
  $ herbc ../../../../../test/typing_negative/012_assignment_unit.herb
  File '../../../../../test/typing_negative/012_assignment_unit.herb', line 3, column 9:
  Error: Type error
  Expected 'unit' but got 'string'
  [1]
  $ herbc ../../../../../test/typing_negative/013_arguments.herb
  File '../../../../../test/typing_negative/013_arguments.herb', line 4, column 6:
  Error: Type error
  Expected 'int' but got 'string'
  [1]
  $ herbc ../../../../../test/typing_negative/014_number_of_arguments.herb
  File '../../../../../test/typing_negative/014_number_of_arguments.herb', line 4, column 4:
  Error: Expected 2 arguments, but received 3
  [1]
  $ herbc ../../../../../test/typing_negative/015_block_type_inference_fun.herb
  File '../../../../../test/typing_negative/015_block_type_inference_fun.herb', line 3, column 9:
  Error: Type error
  Expected '(int) -> int' but got '(string) -> string'
  [1]
  $ herbc ../../../../../test/typing_negative/017_counter.herb
  File '../../../../../test/typing_negative/017_counter.herb', line 16, column 14:
  Error: Type error
  Expected 'int' but got 'string'
  [1]
  $ herbc ../../../../../test/typing_negative/018_entry_return_type_error.herb
  File '../../../../../test/typing_negative/018_entry_return_type_error.herb', line 1, column 0:
  Error: Entry return type error
  Entry must return a value of type 'unit' or 'int' but got 'string'
  [1]
  $ herbc ../../../../../test/typing_negative/019_entry_return_type_error2.herb
  File '../../../../../test/typing_negative/019_entry_return_type_error2.herb', line 1, column 0:
  Error: Entry return type error
  Entry must return a value of type 'unit' or 'int' but got '() -> int'
  [1]





