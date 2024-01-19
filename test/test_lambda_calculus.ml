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

let substitution_tests =
  "Substitute tests"
  >::: [
         ( "ex1" >:: fun _ ->
           assert_equal true
             (alpha_equivalence (parse "y")
                (substitute (parse "x") (parse "x") (parse "y"))) );
         ( "ex2" >:: fun _ ->
           assert_equal true
             (alpha_equivalence (parse "z y")
                (substitute (parse "x y") (parse "x") (parse "z"))) );
         ( "ex3" >:: fun _ ->
           assert_equal true
             (alpha_equivalence (parse "\\x.x y")
                (substitute (parse "\\x.x y") (parse "x") (parse "y"))) );
         ( "ex4" >:: fun _ ->
           assert_equal true
             (alpha_equivalence (parse "\\z.x")
                (substitute (parse "\\x.y") (parse "y") (parse "x"))) );
         ( "ex5" >:: fun _ ->
           assert_equal true
             (alpha_equivalence (parse "\\z.z x")
                (substitute (parse "\\x.x y") (parse "y") (parse "x"))) );
       ]

let normal_order_evaluation_tests =
  "Normal order evaluation tests"
  >::: [
         ( "ex1" >:: fun _ ->
           assert_equal true
             (alpha_equivalence (parse "\\x.x")
                (normal_order_evaluation (parse "\\z.(\\x.x) z"))) );
         ( "ex2" >:: fun _ ->
           assert_equal true
             (alpha_equivalence (parse "foo \\x.x")
                (normal_order_evaluation (parse "foo \\z.(\\x.x) z"))) );
         ( "ex3" >:: fun _ ->
           assert_equal true
             (alpha_equivalence (parse "\\y.(\\z.z)")
                (normal_order_evaluation (parse "\\y.(\\z.(\\x.x) z)"))) );
         ( "ex4" >:: fun _ ->
           assert_equal true
             (alpha_equivalence (parse "(foo \\x.x) bar")
                (normal_order_evaluation (parse "(foo \\z.(\\x.x) z) bar"))) );
       ]

let call_by_name_evaluation_tests =
  "Call by name evaluation tests"
  >::: [
         ( "ex1" >:: fun _ ->
           assert_equal true
             (alpha_equivalence
                (parse "(\\x.x) (\\z.(\\x.x) z)")
                (call_by_name_evaluation
                   (parse "(\\x.x) ((\\x.x) (\\z.(\\x.x) z))"))) );
         ( "ex2" >:: fun _ ->
           assert_equal true
             (alpha_equivalence (parse "\\z.(\\x.x) z")
                (call_by_name_evaluation (parse "\\z.(\\x.x) z"))) );
         ( "ex3" >:: fun _ ->
           assert_equal true
             (alpha_equivalence (parse "x y")
                (call_by_name_evaluation (parse "x y"))) );
       ]

let _ =
  run_test_tt_main parser_tests;
  run_test_tt_main free_variables_tests;
  run_test_tt_main substitution_tests;
  run_test_tt_main normal_order_evaluation_tests;
  run_test_tt_main call_by_name_evaluation_tests
