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

let counter = ref 0

(* currently using counter and "q" to generate a fresh variable. But this
   implementation is flawed, as "q1" might already present in LC expression.
   For now, I assume there is no "q" or any var with "q" in LC expression.
   TODO: Think of better way to generate fresh variables.
*)
let fresh_varaible () =
  counter := !counter + 1;
  "q" ^ string_of_int !counter

let substitute expr var replace_term =
  let free_vars = free_variables replace_term in
  let expr = parse expr in
  let var = parse var in
  let replace_term = parse replace_term in
  let rec subst_helper e v r =
    match (e, v) with
    | Var x, Var y -> if x = y then r else Var x
    | (Abstraction (x, e1) as l_term), Var y ->
        if x = y then l_term
        else if List.mem x free_vars then
          let fv = fresh_varaible () in
          let new_expr = Abstraction (fv, subst_helper e1 (Var x) (Var fv)) in
          subst_helper new_expr (Var y) replace_term
        else Abstraction (x, subst_helper e1 v r)
    | Application (e1, e2), Var _ ->
        Application (subst_helper e1 v r, subst_helper e2 v r)
    | _, _ -> failwith "Unexpected substitution!"
  in
  subst_helper expr var replace_term
