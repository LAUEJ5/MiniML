(*
                          MiniML -- Testing
*)                           
open Expr ;;
open Evaluation ;;
open Evaluation.Env ;;
open CS51Utils ;;
open Absbook ;;

let free_vars_test () =
  unit_test (same_vars (free_vars (Num 5)) (vars_of_list []))
  "free_vars num";
  unit_test (same_vars (free_vars (Var "a")) (vars_of_list ["a"]))
  "free_vars variable";
  unit_test (same_vars (free_vars (Unop (Negate, Var "x"))) 
             (vars_of_list ["x"]))
  "free_vars unop";
  unit_test (same_vars (free_vars (Binop (Plus, Num 5, Num 6))) 
             (vars_of_list []))
  "free_vars binop 1";
  unit_test (same_vars (free_vars (Binop (Equals, Var "x", String "str"))) 
             (vars_of_list ["x"]))
  "free_vars binop 2";
  unit_test (same_vars (free_vars (Fun ("x", Bool false))) 
             (vars_of_list []))
  "free_vars fun 1";
  unit_test (same_vars (free_vars (Fun ("x", Binop 
             (Times, Var "x", Var "y")))) (vars_of_list ["y"]))
  "free_vars fun 2";
  unit_test (same_vars (free_vars (Let ("h", Num 6, Num 8))) 
             (vars_of_list []))
  "free_vars let 1";
  unit_test (same_vars (free_vars (Let ("x", Num 6, Binop 
             (Minus, Var "x", Var "y")))) (vars_of_list ["y"]))
  "free_vars let 2";
  unit_test (same_vars (free_vars (Letrec ("x", Var "y", Binop 
             (Times, Var "x", Var "y")))) (vars_of_list ["y"]))
  "free_vars letrec 1";
  unit_test (same_vars (free_vars (Letrec ("o", Fun ("p", Var "o"), Var "o")))
             (vars_of_list []))
  "free_vars letrec 2";
  unit_test (same_vars (free_vars (App (Fun ("x", Var  "x"), 
             Fun ("y", Var "x")))) (vars_of_list ["x"]))
  "free_vars app";;

let subst_test () = 
  unit_test (subst "x" (Num 1) (Var "x") = Num 1)
  "subst same var";
  unit_test (subst "x" (Num 2) (Var "y") = Var "y")
  "subst different var";
  unit_test (subst "x" (Num 3) (Fun ("x", Binop (Plus, Var "x", Num 3))) = 
             Fun ("x", Binop (Plus, Var "x", Num 3)))
  "subst fun";
  unit_test (subst "x" (Num 2) (Let ("x", Binop (Times, Var "x", Num 3), 
             Binop (Divides, Var "x", Num 4))) = Let ("x", 
             Binop (Times, Num 2, Num 3), Binop (Divides, Var "x", Num 4)))
  "subst let";
  unit_test (subst "x" (Num 2) (Letrec ("x", Binop (Minus, Var "x", Num 3), 
             Binop (Plus, Var "x", Num 4))) = Letrec ("x", 
             Binop (Minus, Var "x", Num 3), Binop (Plus, Var "x", Num 4)))
  "subst letrec 1";
  unit_test (subst "x" (Num 8) (Letrec ("x", Fun ("y", Var "x"), Var "x")) = 
             Letrec ("x", Fun ("y", Var "x"), Var "x"))
  "subst letrec 2";
  unit_test (subst "x" (Num 4) (Conditional (Binop (Equals, Var "x", Num 4), 
             Num 1, Binop (Plus, Var "x", Num 2))) = Conditional (Binop 
             (Equals, Num 4, Num 4), Num 1, Binop (Plus, Num 4, Num 2)))
  "subst conditional";
  unit_test (subst "x" (Num 7) (App (Fun ("x", Var  "x"), Fun ("y", Var "x"))) = 
             App (Fun ("x", Var  "x"), Fun ("y", Num 7)))
  "subst app";;

  
    let eval_s_test () = 
      let evaluate (exp) = 
        let env = Env.empty () in
        eval_s exp env
      in
      unit_test (try evaluate (Var "a") = Val (Var "a")
                 with 
                 | EvalError _ -> true
                 | _ -> false)
      "eval_s var";
      unit_test (evaluate (Num 5) = Val (Num 5))
      "eval_s num";
      unit_test (evaluate (Bool true) = Val (Bool true))
      "eval_s bool";
      unit_test (evaluate (Unop (Negate, Num 4)) = Val (Num ~-4))
      "eval_s unop valid";
      unit_test (try evaluate (Unop (Negate, Var "x")) = Val (Var "~-x")
                 with 
                 | EvalError _ -> true
                 | _ -> false)
      "eval_s unop invalid";
      unit_test (evaluate (Binop (Minus, Num 1, Num 2)) = Val (Num ~-1))
      "eval_s binop int operations valid";
      unit_test (evaluate (Binop (Equals, Num 3, Num 7)) = Val (Bool false))
      "eval_s binop equals nums";
      unit_test (evaluate (Binop (Equals, Bool false, Bool false)) = Val (Bool true))
      "eval_s binop equals bools";
      unit_test (try evaluate (Binop (Minus, Bool true, Bool false)) = 
                                                              Val (Var "true-false")
                 with 
                 | EvalError _ -> true
                 | _ -> false)
      "eval_s binop - invalid";
      unit_test (evaluate (Conditional (Bool true, String "str", Num 1)) = 
                                                        Val (String "str"))
      "eval_s conditional";
      unit_test (evaluate (Fun ("x", Var "x")) = Val (Fun ("x", Var "x")))
      "eval_s fun";
      unit_test (evaluate (Let ("x", Num 1, Binop (Plus, Num 5, Var "x"))) 
                  = Val (Num 6))
      "eval_s let";
      unit_test (evaluate (Letrec ("f", Fun ("x", Conditional (Binop 
                  (Equals, Var "x", Num 0), Num 1, Binop (Times, Var "x", 
                  App (Var "f", Binop (Minus, Var "x", Num 1))))), 
                  App (Var "f", Num 4))) = Val (Num 24))
      "eval_s letrec";
      unit_test (evaluate (Float 1.) = Val (Float 1.))
      "eval_s float";
      unit_test (evaluate (Binop (Fl_minus, Float 5., Float 2.)) = Val (Float (3.)))
      "eval_s binop float operations";
      unit_test (evaluate (Binop (Equals, Float 1., Float 1.)) = Val (Bool true))
      "eval_s binop float equals";
      unit_test (evaluate (Binop (Concat, String "str", String "str"))
                  = Val (String "strstr"))
      "eval_s binop concat";
      unit_test (try evaluate (Binop (Fl_plus, Num 1, Num 7)) = Val (Num ~-6)
                 with 
                 | EvalError _ -> true
                 | _ -> false)
      "eval_s binop invalid floats";;
    
    let env_test () = 
      let env1 = empty () in
      unit_test (close (Num 1) env1 = Closure (Num 1, env1))
      "Env closure";
      let env1 = extend env1 "x" (ref (Val (Num 1))) in
      unit_test (env_to_string env1 = "NAME: x VALUE: 1\n")
      "Env extend";
      unit_test (value_to_string (lookup env1 "x") = "1")
      "Env lookup 1";
      unit_test (try value_to_string (lookup env1 "y") = ""
                 with 
                 | EvalError _ -> true
                 | _ -> false)
      "Env lookup 2";;
    
    let eval_d_test () =
      let evaluate (ex) = 
        let env = Env.empty () in
        eval_d ex env
      in
      unit_test (try (evaluate (Var "x")  = Val (Var "x "))
                 with 
                 | EvalError _ -> true
                 | _ -> false)
      "eval_d free var";
      let env1 = extend (Env.empty ()) "x" (ref (Val (Num 1))) in
      unit_test (eval_d (Var "x") env1 = Val (Num 1))
      "eval_d not free var";
      unit_test (eval_d (Unop (Negate, Num 5)) env1 = Val (Num ~-5))
      "eval_d unop";
      unit_test (eval_d (Unop (Negate, Var "x")) env1 = Val (Num ~-1))
      "eval_d lookup";
      unit_test (eval_d (Binop (Plus, Num 4, Num 5)) env1 = Val (Num 9))
      "eval_d binop valid";
      unit_test (eval_d (Binop (Equals, Bool true, Bool true)) env1 = 
                                                                  Val (Bool true))
      "eval_d binop equals bools";
      unit_test (try eval_d (Binop (Minus, Var "x", Var "y")) env1 = Val (Var "x-y")
                 with 
                 | EvalError _ -> true
                 | _ -> false)
      "eval_d binop invalid";
      let env2 = extend env1 "y" (ref (Val (Num 2))) in
      unit_test (eval_d (Binop (Times, Var "x", Var "y")) env2 = Val (Num 2))
      "eval_d lookup";
      unit_test (eval_d (Conditional (Bool false, Num 1, Num 2)) env1 = Val (Num 2))
      "eval_d conditional else";
      unit_test (eval_d (Conditional (Bool true, Var "x", Var "y")) env2 
                  = Val (Num 1))
      "eval_d conditional lookup";
      unit_test (eval_d (Fun ("x", Var "x")) env1 = Val (Fun ("x", Var "x")))
      "eval_d fun";
      unit_test (eval_d (Fun ("x", Var "x")) env2 = Val (Fun ("x", Var "x")))
      "eval_d fun with dyn var";
      unit_test (eval_d (Let ("x", Num 1, Binop (Divides, Num 2, Var "x"))) env1
                  = Val (Num 2))
      "eval_d let";
      unit_test (eval_d (Let ("z", Num 4, Binop (Minus, Var "z", Var "y"))) env2
                  = Val (Num 2))
      "eval_d let lookup";
      unit_test (eval_d (Letrec ("f", Fun ("x", Conditional (Binop 
                  (Equals, Var "x", Num 0), Num 1, Binop (Times, Var "x", 
                  App (Var "f", Binop (Minus, Var "x", Num 1))))), 
                  App (Var "f", Num 4))) env1 = Val (Num 24))
      "eval_d letrec 1";
      unit_test (eval_d (Letrec ("x", Num 7, Binop (Minus, Var "x", Var "y"))) env2
                  = Val (Num 5))
      "eval_d letrec lookup";
      unit_test (eval_d (Float 1.) env1 = Val (Float 1.))
      "eval_d float 1";
      unit_test (eval_d (Binop (Fl_times, Float 9., Float 9.)) env1 = 
                                                                    Val (Float 81.))
      "eval_d binop - Fl_times";
      unit_test (eval_d (Binop (Equals, Float 1., Float 2.)) env1 = 
                                                                  Val (Bool false))
      "eval_d binop equals floats";
      unit_test (eval_d (Binop (Concat, String "str", String "str")) env1
                  = Val (String "strstr"))
      "eval_d binop concat";
      unit_test (try eval_d (Binop (Concat, Float 1., Float 7.)) env1
                      = Val (Float 1.7)
                 with 
                 | EvalError _ -> true
                 | _ -> false)
      "eval_d string invalid concat";;
    
    let eval_l_test () = 
      let evaluate (exp) = 
        let env = Env.empty () in
        eval_l exp env
      in
        unit_test (try evaluate (Var "x") = Val (Var "x")
                   with 
                   | EvalError _ -> true
                   | _ -> false)
        "eval_l free var";
        let env1 = extend (Env.empty ()) "x" (ref (Val (Num 1))) in
        let env2 = extend env1 "x" (ref (Val (Num 2))) in
        unit_test (evaluate (Fun ("x", Var "x")) 
                    = Closure (Fun ("x", Var "x"), Env.empty ()))
        "eval_l fun";
        unit_test (eval_l (Fun ("x", Var "x")) env2 
                    = Closure (Fun ("x", Var "x"), env2))
        "eval_l fun var in environment";
        unit_test (evaluate (Let ("x", Num 9, Binop (Times, Num 9, Var "x"))) 
                    = Val (Num 81))
        "eval_l let";
        unit_test (eval_l (Let ("y", Num 4, Binop (Plus, Var "y", Var "x"))) env2
                    = Val (Num 6))
        "eval_l let lookup";
        unit_test (evaluate (Letrec ("x", Num 1, Binop (Plus, Num 8, Var "x"))) 
        = Val (Num 9))
        "eval_l letrec";
        unit_test (evaluate (Let ("x", Num 1, Let ("f", Fun ("y", 
                    Binop (Plus, Var "x", Var "y")), Let ("x", Num 2, 
                    App (Var "f", Num 3))))) = Val (Num 4))
        "eval_l - lex compare";;
    
      let eval_e_test () = 
        let env = Env.empty () in
        unit_test ((eval_e (Ref ("x", Num 0)) env) = 
                          (Val (Unit ()), (Env.extend env "x" (ref (Val (Num 0))))))
        "eval_e ref num";
        unit_test (eval_e (Ref ("y", Bool true)) env = 
                      (Val (Unit ()), Env.extend env "y" (ref (Val (Bool true)))))
        "eval_e ref bool";
        unit_test (eval_e (Ref ("z", (Let ("x", Num 1, Binop 
                            (Divides, Num 2, Var "x"))))) env = 
                            (Val (Unit ()), Env.extend env "z" (ref (Val (Num 2)))))
        "eval_e ref complex";
        let env1 = extend env "x" (ref (Val (Num 0))) in
        let env2 = extend env1 "y" (ref (Val (Bool true))) in
        let env3 = extend env2 "z" (ref (Val (Num 3))) in
        unit_test (eval_e (Deref "x") env3 = (Val (Num 0), env3))
        "eval_e deref x";
        unit_test (eval_e (Deref "y") env3 = (Val (Bool true), env3))
        "eval_e deref y";
        unit_test (eval_e (Deref "z") env3 = (Val (Num 3), env3))
        "eval_e deref z";
        ;;

let test_all () =
  free_vars_test () ;
  subst_test () ;
  eval_s_test () ;
  env_test () ;
  eval_d_test () ;
  eval_l_test () ;
  eval_e_test () ;
  () ;;

let _ = test_all () ;;
