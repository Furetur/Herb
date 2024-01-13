  $ herbc ../../../../../test/io/000_empty_entry.herb -o main && ./main
  $ herbc ../../../../../test/io/001_print_1337.herb -o main && ./main
  1337
  $ herbc ../../../../../test/io/002_println_numbers.herb -o main && ./main
  1
  2
  3
  $ herbc ../../../../../test/io/003_print_numbers.herb -o main && ./main
  1234
  $ herbc ../../../../../test/io/004_assert_truthful.herb -o main && ./main
  $ herbc ../../../../../test/io/005_assert_falseful.herb -o main && ./main
  Assertion failed!
  [1]
