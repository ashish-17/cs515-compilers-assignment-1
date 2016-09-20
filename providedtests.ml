open Assert
open Hellocaml

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)
let make_list n = let rec make_list_h n l = if n > 0 then make_list_h (n-1) ([n] @ l) else l in
  make_list_h n []

let big_list = make_list 10_000_000

let excp_empty_list = Failure "Empty List"

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

  Test ("Test for Problem 3-4 (Tail recursion)", [
    ("case1", assert_eqf(fun () -> list_head (rev_t big_list)) 10000000);
  ]);
] 
