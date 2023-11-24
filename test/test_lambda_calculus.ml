open OUnit2
open LambdaCalculus.Lci
open LambdaCalculus.Ast

let parser_tests =
  "LC parser tests"
  >::: [
         ("ex1" >:: fun _ -> assert_equal (Var "x") (parse "x"));
         ( "ex2" >:: fun _ ->
           assert_equal (Abstraction ("x", Var "x")) (parse "\\x.x") );
         ( "ex3" >:: fun _ ->
           assert_equal (Application (Var "x", Var "y")) (parse "x y") );
         ( "ex4" >:: fun _ ->
           assert_equal
             (Abstraction
                ("x", Application (Var "x", Application (Var "y", Var "z"))))
             (parse "\\x.x y z") );
         ( "ex5" >:: fun _ ->
           assert_equal
             (Abstraction
                ("x", Application (Var "x", Application (Var "y", Var "z"))))
             (parse "(\\x.(x (y z)))") );
         ( "ex6" >:: fun _ ->
           assert_equal
             (Abstraction
                ( "x23",
                  Application (Var "x", Application (Var "y45rf", Var "z")) ))
             (parse "\\x23.x y45rf z") );
       ]

let free_variables_tests =
  "Free variables tests"
  >::: [
         ( "ex1" >:: fun _ ->
           assert_equal [ "x"; "y" ] (free_variables (parse "x y")) );
         ("ex2" >:: fun _ -> assert_equal [] (free_variables (parse "\\x.x")));
         ( "ex3" >:: fun _ ->
           assert_equal [ "y" ] (free_variables (parse "\\x.y")) );
         ( "ex4" >:: fun _ ->
           assert_equal [ "y" ] (free_variables (parse "\\x.(\\y.x) y")) );
         ( "ex5" >:: fun _ ->
           assert_equal [ "y" ] (free_variables (parse "\\x.(\\y.y x) y")) );
       ]

(* Not a better way to write subst tests, as this is relying on some random
   fresh variable that assumes to be constant every time.
   TODO: Replace these tests with by checking alpha equivalence *)
let substitution_tests =
  "Substitute tests"
  >::: [
         ( "ex1" >:: fun _ ->
           assert_equal (Var "y")
             (substitute (parse "x") (parse "x") (parse "y")) );
         ( "ex2" >:: fun _ ->
           assert_equal
             (Application (Var "z", Var "y"))
             (substitute (parse "x y") (parse "x") (parse "z")) );
         ( "ex3" >:: fun _ ->
           assert_equal
             (Abstraction ("x", Application (Var "x", Var "y")))
             (substitute (parse "\\x.x y") (parse "x") (parse "y")) );
         ( "ex4" >:: fun _ ->
           assert_equal
             (Abstraction ("q1", Var "x"))
             (substitute (parse "\\x.y") (parse "y") (parse "x")) );
       ]

let _ =
  run_test_tt_main parser_tests;
  run_test_tt_main free_variables_tests;
  run_test_tt_main substitution_tests
