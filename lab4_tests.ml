open Lab4;;
open CS51;;

(*Part I Unit Tests*)
let test_curry () =
	unit_test((((curry (+)) 3) 0) = 3)   "test_curry add";;
let test_uncurry () =
	unit_test((uncurry ((+), 3, 0)) = 3)   "test_uncurry add";;
let test_plus () =
	unit_test((plus (2, 3)) = 5) "plus positive";;
let test_times () =
	unit_test((times (2, 3)) = 6) "times positive";;
let test_prods () =
	unit_test((prods [(1,2);(2,3);(3,5)]) = [2;6;15]) "prods positive";;
let test_max_list () =
	unit_test ((max_list_opt []) = None) "max_list empty";;

let _ = test_curry () ;;
let _ = test_uncurry ();;
let _ = test_plus ();;
let _ = test_times ();;
let _ = test_prods () ;;
let _ = test_max_list () ;;