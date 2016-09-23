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

  Test ("Student-Provided Tests for Problem 5", [
    ("case 1", assert_eqf(fun() -> run [] (compile e1)) 6l);
    ("case 2", assert_eqf(fun() -> run [("x", 3l)] (compile e2)) 4l);
    ("case 3", assert_eqf(fun() -> run [("x", 2l); ("y", 5l)] (compile e3)) (-45l));
    ("case 4", assert_eqf(fun() -> run [] (compile test_excp1)) 90l);
    ("case 5", assert_eqf(fun() -> run [("x", 10l); ("y", 10l)] (compile test_excp2)) 1000l);
    ("case 6", assert_eqf(fun() -> run [("x", 0l); ("y", 10l); ("z", 100l)] (compile test_excp3)) 1090l);
  ]);
] 
