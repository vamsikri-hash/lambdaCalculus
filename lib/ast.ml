type expr =
  | Var of string
  | Abstraction of (string * expr)
  | Application of (expr * expr)
