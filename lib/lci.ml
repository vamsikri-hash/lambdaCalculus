open Ast

let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let free_variables s =
  let rec free_helper acc = function
    | Var x -> x :: acc
    | Abstraction (x, e) ->
        List.filter (fun ele -> ele <> x) (free_helper acc e)
    | Application (e1, e2) -> free_helper acc e1 @ free_helper acc e2
  in
  s |> parse |> free_helper []
