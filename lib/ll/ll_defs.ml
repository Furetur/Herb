module T = Ollvm.Ez.Type

let int_t = T.i32
let int_ptr_t = T.pointer int_t
let bool_t = T.i1
let ( <-- ) = Ollvm.Ez.Instr.( <-- )
