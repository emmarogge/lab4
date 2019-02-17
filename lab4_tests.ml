open Lab4;;
open CS51;;

(*Part I Unit Tests*)
(* let test_curry () =
	unit_test ((curry (+) (3, 0)) = 3)   "curry add";;

let test_uncurry () =
	unit_test ((uncurry ((+), 3, 0)) = 3)   "uncurry add";; *)

let test_plus () =
	unit_test ((plus (2, 3)) = 5) "plus positive";;

let test_times () =
	unit_test ((times (2, 3)) = 6) "times positive";;

let test_prods () =
	unit_test ((prods [(1,2);(2,3);(3,5)]) = [2;6;15]) "prods positive";;

let test_max_list_opt () =
	unit_test ((max_list_opt []) = None) "max_list_opt empty";
	unit_test ((max_list_opt [1;3;~-2]) = Some 3) "max_list_opt valid";;

let test_max_list () =
	unit_test ((max_list_opt []) = None) "max_list_opt empty";
	unit_test ((max_list_opt [1;3;~-2]) = Some 3) "max_list_opt valid";;

let test_min_option () =
	unit_test ((min_option None None) = None) "min_option none";
	unit_test ((min_option None (Some 7)) = Some 7) "min_option one";;

let test_plus_option () =
	unit_test ((plus_option None None) = None) "plus_option none";
	unit_test ((plus_option None  (Some 7) ) = (Some 7)) "plus_option one_valid";
	unit_test ((plus_option (Some 8)  (Some 7) ) = (Some 15)) "plus_option both_valid";;

(*Part III Unit Tests*)
let test_calc_option () =
	unit_test ((calc_option (+) (Some 1) None) = (Some 1)) "calc_option only_left";
	unit_test ((calc_option (-) None (Some 3)) = (Some 3)) "calc_option only_right";
	unit_test ((calc_option ( * ) None None) = None) "calc_option double_none";
	unit_test ((calc_option ( * ) (Some 3) (Some 5)) = (Some 15)) "calc_option both_valid";;

let test_min_option_2 () =
	unit_test ((min_option_2 None None) = None) "min_option_2 none";
	unit_test ((min_option_2 None (Some 7)) = Some 7) "min_option_2 one";
	unit_test ((min_option_2 (Some 10) (Some 7)) = Some 7) "min_option_2 both_valid";;

let test_plus_option_2 () =
	unit_test ((plus_option_2 None None) = None) "plus_option_2 none";
	unit_test ((plus_option_2 None  (Some 7) ) = (Some 7)) "plus_option_2 one_valid";
	unit_test ((plus_option_2 (Some ~-2) (Some 7) ) = (Some 5)) "plus_option_2 both_valid";;

let test_and_option () =
	unit_test ((and_option None None) = None) "and_option none";
	unit_test ((and_option None (Some (1 > 2))) = Some false) "and_option one_valid";
	unit_test ((and_option (Some (1 < 2)) (Some (2 < 3))) = Some true) "and_option both_valid";;

let test_zip_opt () =
	unit_test ((zip_opt [] [1;2;3]) = None) "zip_opt empty"; (*Interesting--empty list are same type *)
	unit_test ((zip_opt [1;2;3] [4;5]) = None) "zip_opt unequal_length";
	unit_test ((zip_opt [1; 2; 3] ['a';'b';'c']) = Some [(1, 'a'); (2, 'b'); (3, 'c')]) "zip_opt equal";;

let test_dotprod () =
	unit_test ((dotprod [1;2;3] [4;5;6]) = (Some 32)) "dotprod both_valid";
	unit_test ((dotprod [] [1;2;3]) = None) "dotprod one_none";
	unit_test ((dotprod [2;3] [1;2;3]) = None) "dotprod unequal_length";;

(*Part IV Unit Tests*)

(* Define a function for the purpose of test.*)
let square (x : int) : int =
	x * x;;
let test_maybe () =
	unit_test ((maybe square (Some 3)) = (Some 9) ) "maybe valid";
	unit_test ((maybe square None) = None) "maybe none";;

let test_zip_opt_2 () =
	unit_test ((zip_opt_2 [] [1;2;3]) = None) "zip_opt_2 empty"; (*Interesting--empty list are same type *)
	unit_test ((zip_opt_2 [1;2;3] [4;5]) = None) "zip_opt_2 unequal_length";
	unit_test ((zip_opt_2 [1; 2; 3] ['a';'b';'c']) = Some [(1, 'a'); (2, 'b'); (3, 'c')]) "zip_opt_2 equal";;

let test_max_list_opt_2 () =
	unit_test ((max_list_opt_2 []) = None) "max_list_opt_2 empty";
	unit_test ((max_list_opt_2 [1;2;3;~-1]) = (Some 3)) "max_list_opt_2 all_valid";;


let test_all () =
(* 	test_curry ();
	test_uncurry (); *)
	test_plus ();
	test_times ();
	test_prods ();
	test_max_list_opt ();
	test_min_option ();
	test_plus_option ();
	test_calc_option () ;
	test_min_option_2 ();
	test_plus_option_2 ();
	test_and_option ();
	test_zip_opt (); 
	test_maybe ();
	test_dotprod ();
	test_zip_opt_2 ();
	test_max_list_opt_2 ();;

let _ = test_all ();;