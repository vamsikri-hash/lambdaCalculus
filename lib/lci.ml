open Ast

let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec free_variables = function
  | Var x -> [ x ]
  | Abstraction (x, e) -> List.filter (fun ele -> ele <> x) (free_variables e)
  | Application (e1, e2) -> free_variables e1 @ free_variables e2

let counter = ref 0

(* currently using counter and "q" to generate a fresh variable. But this
   implementation is flawed, as "q1" might already present in LC expression.
   For now, I assume there is no "q" or any var with "q" in LC expression.
   TODO: Think of better way to generate fresh variables.
*)
let fresh_variable () =
  counter := !counter + 1;
  "q" ^ string_of_int !counter

let substitute expr var replace_term =
  let free_vars = free_variables replace_term in
  let rec subst_helper e v r =
    match (e, v) with
    | Var x, Var y -> if x = y then r else Var x
    | (Abstraction (x, e1) as l_term), Var y ->
        if x = y then l_term
        else if List.mem x free_vars then
          let fv = fresh_variable () in
          let new_expr = Abstraction (fv, subst_helper e1 (Var x) (Var fv)) in
          subst_helper new_expr (Var y) r
        else Abstraction (x, subst_helper e1 v r)
    | Application (e1, e2), Var _ ->
        Application (subst_helper e1 v r, subst_helper e2 v r)
    | _, _ -> failwith "Unexpected substitution!"
  in
  subst_helper expr var replace_term

let rec alpha_equivalence expr1 expr2 =
  match (expr1, expr2) with
  | Var x, Var y -> if x = y then true else false
  | Abstraction (x1, e1s1), Abstraction (x2, e2s1) ->
      let fv = fresh_variable () in
      substitute e1s1 (Var x1) (Var fv) = substitute e2s1 (Var x2) (Var fv)
  | Application (e1s1, e1s2), Application (e2s1, e2s2) ->
      alpha_equivalence e1s1 e2s1 && alpha_equivalence e1s2 e2s2
  | _, _ -> false
