open Assert
open Hellocaml

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)
let make_list n = let rec make_list_h n l = if n > 0 then make_list_h (n-1) ([n] @ l) else l in
  make_list_h n []

let big_list = make_list 10_000_000

let excp_empty_list = Failure "Empty List"
let test_excp1 : exp = Mult(Add(Add(Const 4l, Add(Const 5l, Const 7l)), Mult(Const 7l, (Neg (Const 1l)))), Add(Const 1l, Const 9l)) 
let test_excp2 : exp = Mult(Add(Var "x", test_excp1), Var "y")
let test_excp3 : exp = Add(Add(Var "z", test_excp2), test_excp1)
let test_opt_1 : exp = Neg(Neg(Neg (Const 1l)))
let test_optr_1: exp = Const (-1l)
let test_opt_2 : exp = Add(Add(Add(Const 5l, Const 0l), Const 1l), Const 0l)
let test_optr_2: exp = Const 6l
let test_opt_3 : exp = Add(Add(Add(Var "y", Const 0l), Var "x"), Const 0l)
let test_optr_3: exp = Add(Var "y", Var "x")
let test_opt_4 : exp = Mult(Mult(Mult(Var "y", Const 1l), Var "x"), Const 0l)
let test_optr_4: exp = Const 0l
let test_opt_5 : exp = Mult(Mult(Mult(Var "y", Const 1l), Var "x"), Const 1l)
let test_optr_5: exp = Mult(Var "y", Var "x")

let list_head l = 
    begin match l with
    | [] -> raise excp_empty_list
    | h::t -> h
    end

let provided_tests : suite = [
  Test ("Student-Provided Tests For Problem 1-3", [

   ("case1", assert_eq prob3_ans 42); 
   ("case2", assert_eqf (fun () -> prob3_case2 17) 25 ); 
   ("case3", assert_eqf (fun () -> prob3_case3) 64); 
  ]);

  Test ("Student-Provided Tests for Problem 3-4 (Tail recursion)", [
    ("case1", assert_eqf(fun () -> list_head (rev_t big_list)) 10000000);
  ]);

  Test ("Student-Provided Tests for Problem 4-3", [
    ("case 1", assert_eqf(fun() -> interpret [] e1) 6l);
    ("case 2", assert_eqf(fun() -> interpret [("x", 3l)] e2) 4l);
    ("case 3", assert_eqf(fun() -> interpret [("x", 2l); ("y", 5l)] e3) (-45l));
    ("case 4", assert_eqf(fun() -> interpret [] test_excp1) 90l);
    ("case 5", assert_eqf(fun() -> interpret [("x", 10l); ("y", 10l)] test_excp2) 1000l);
    ("case 6", assert_eqf(fun() -> interpret [("x", 0l); ("y", 10l); ("z", 100l)] test_excp3) 1090l);
  ]);

  Test ("Student-Provided Tests for Problem 4-4", [
    ("case 1", assert_eqf(fun() -> run [] (compile (optimize e1))) 6l);
    ("case 2", assert_eqf(fun() -> run [("x", 3l)] (compile (optimize e2))) 4l);
    ("case 3", assert_eqf(fun() -> run [("x", 2l); ("y", 5l)] (compile (optimize e3))) (-45l));(*
    ("case 4", assert_eqf(fun() -> run [] (compile (optimize test_excp1))) 90l);
    ("case 5", assert_eqf(fun() -> run [("x", 10l); ("y", 10l)] (compile (optimize test_excp2))) 1000l);
    ("case 6", assert_eqf(fun() -> run [("x", 0l); ("y", 10l); ("z", 100l)] (compile (optimize test_excp3))) 1090l);*)
    ("case 7", assert_eqf(fun() -> (optimize test_opt_1)) test_optr_1);
    ("case 8", assert_eqf(fun() -> (optimize test_opt_2)) test_optr_2);
    ("case 9", assert_eqf(fun() -> (optimize test_opt_3)) test_optr_3);
    ("case 10", assert_eqf(fun() -> (optimize test_opt_4)) test_optr_4);
    ("case 11", assert_eqf(fun() -> (optimize test_opt_5)) test_optr_5);
  ]);

  Test ("Student-Provided Tests for Problem 5", [
    ("case 1", assert_eqf(fun() -> run [] (compile e1)) 6l);
    ("case 2", assert_eqf(fun() -> run [("x", 3l)] (compile e2)) 4l);
    ("case 3", assert_eqf(fun() -> run [("x", 2l); ("y", 5l)] (compile e3)) (-45l));
    ("case 4", assert_eqf(fun() -> run [] (compile test_excp1)) 90l);
    ("case 5", assert_eqf(fun() -> run [("x", 10l); ("y", 10l)] (compile test_excp2)) 1000l);
    ("case 6", assert_eqf(fun() -> run [("x", 0l); ("y", 10l); ("z", 100l)] (compile test_excp3)) 1090l);
  ]);
] 
