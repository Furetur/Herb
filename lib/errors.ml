open Base

type error = string
type 'a compilation_result = ('a, error) Result.t
