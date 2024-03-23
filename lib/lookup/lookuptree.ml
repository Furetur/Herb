open Base

(* ----- Expressions ----- *)

type expr =
  | Constant of Parsetree.constant
  | Ident of Ident.ident
  | Binop of expr * Parsetree.binop * expr
  | Call of { callee : expr; args : expr list }
  | Builtin of builtin
[@@deriving show]

and builtin = Print of expr | Println of expr | Assert of expr
[@@deriving show]

(* ----- Declarations ----- *)
and let_decl = Ident.ident * expr [@@deriving show]

(* ----- Statements ----- *)
and stmt =
  | LetDecl of let_decl
  | Assign of expr * expr
  | ExprStmt of expr
  | If of expr * block * block
  | While of expr * block
  | Return of expr
[@@deriving show]

and block = { scope_id : Ident.scope_id; stmts : stmt list } [@@deriving show]

(* ----- Parse Tree ----- *)

type lookuptree = { entry : block } [@@deriving show]
