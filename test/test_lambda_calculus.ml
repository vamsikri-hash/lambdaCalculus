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
         ( "ex4" >:: fun _ ->
           assert_equal
             (Abstraction
                ("x", Application (Var "x", Application (Var "y", Var "z"))))
             (parse "(\\x.(x (y z)))") );
       ]

let free_variables_tests =
  "Free variables tests"
  >::: [
         ("ex1" >:: fun _ -> assert_equal [ "x"; "y" ] (free_variables "x y"));
         ("ex2" >:: fun _ -> assert_equal [] (free_variables "\\x.x"));
         ("ex3" >:: fun _ -> assert_equal [ "y" ] (free_variables "\\x.y"));
         ( "ex4" >:: fun _ ->
           assert_equal [ "y" ] (free_variables "\\x.(\\y.x) y") );
         ( "ex5" >:: fun _ ->
           assert_equal [ "y" ] (free_variables "\\x.(\\y.y x) y") );
       ]

let _ =
  run_test_tt_main parser_tests;
  run_test_tt_main free_variables_tests
