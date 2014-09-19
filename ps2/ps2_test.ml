(* Charles Tark (cyt25) and Lillian Chen (qc53) *)
(* September 18, 2014 *)

open Assertions
open Ps2

(* PROBLEM 1 *)
(* Exercise 1 *)
TEST_UNIT "count_ops_test1" = assert_true ((count_ops (Binop ((+), Val 2, Val 3))) = 1)
TEST_UNIT "count_ops_test2" = assert_true ((count_ops (Binop ((+), Val 3, Unop ((~-), Binop ((/), Val 5, Val 2))))) = 3)
TEST_UNIT "count_ops_test3" = assert_true ((count_ops (Unop ((~-), Val 2))) = 1)
TEST_UNIT "count_ops_test4" = assert_true ((count_ops (Unop ((~-.), Binop ((/.), Val 1., Unop ((~-.), Binop ((+.), Val 3., Val 1.)))))) = 4)

(* Exercise 2 *)
TEST_UNIT "make_fact_tree_test1" = assert_true ((make_fact_tree 3) = Binop (( * ), Val 3, Binop (( * ), Val 2, Binop (( * ), Val 1, Val 1))))
TEST_UNIT "make_fact_tree_test2" = assert_true ((make_fact_tree 1) = Binop (( * ), Val 1, Val 1))
TEST_UNIT "make_fact_tree_test3" = assert_true ((make_fact_tree 2) = Binop (( * ), Val 2, Binop (( * ), Val 1, Val 1)))
TEST_UNIT "make_fact_tree_test4" = assert_true ((make_fact_tree 0) = Val 1)
TEST_UNIT "make_fact_tree_test5" = assert_true ((make_fact_tree 10) = Binop (( * ), Val 10,
	Binop (( * ), Val 9,                                                           
	  Binop (( * ), Val 8,
	   Binop (( * ), Val 7,
	    Binop (( * ), Val 6,
	     Binop (( * ), Val 5,
	      Binop (( * ), Val 4,
	       Binop (( * ), Val 3,
	       	Binop (( * ), Val 2,
	       		Binop (( * ), Val 1, Val 1)))))))))))

(* Exercise 3 *)
TEST_UNIT "eval_test1" = assert_true ((eval (Unop ((~-), Val 5))) = (-5))
TEST_UNIT "eval_test2" = assert_true ((eval (make_fact_tree 5)) = 120)
TEST_UNIT "eval_test3" = assert_true ((eval (make_fact_tree 7)) = 5040)
TEST_UNIT "eval_test4" = assert_true ((eval (Binop ((+), Val 2, Val 3))) = 5)
TEST_UNIT "eval_test5" = assert_true ((eval (Binop ((+), Val 3, Unop ((~-), Binop ((/), Val 5, Val 2))))) = 1)
TEST_UNIT "eval_test6" = assert_true ((eval (Unop ((~-), Val 2))) = (-2))
TEST_UNIT "eval_test7" = assert_true ((eval (Unop ((~-.), Binop ((/.), Val 1., Unop ((~-.), Binop ((+.), Val 3., Val 1.)))))) = 0.25)

(* PROBLEM 2 *)
(* Exercise 1 *)
TEST_UNIT "product_test1" = assert_true ((product [777.5;4]) = 3110.)
TEST_UNIT "product_test2" = assert_true ((product []) = 1.)
TEST_UNIT "product_test3" = assert_true ((product [0.0;0.0]) = 0.0)
TEST_UNIT "product_test4" = assert_true ((product [1.0;1.0;2.0;3.0]) = 6.)

(* Exercise 2 *)
TEST_UNIT "concat_left_test1" = assert_true ((concat_left ["cs";"3110"]) = "cs3110")
TEST_UNIT "concat_left_test2" = assert_true ((concat_left ["";"";""]) = "")
TEST_UNIT "concat_left_test3" = assert_true ((concat_left []) = "")
TEST_UNIT "concat_left_test4" = assert_true ((concat_left ["hello";"world";"!"]) = "helloworld!")

TEST_UNIT "concat_right_test1" = assert_true ((concat_right ["cs";"3110"]) = "cs3110")
TEST_UNIT "concat_right_test2" = assert_true ((concat_right ["";"";""]) = "")
TEST_UNIT "concat_right_test3" = assert_true ((concat_right []) = "")
TEST_UNIT "concat_right_test4" = assert_true ((concat_right ["hello";"world";"!"]) = "helloworld!")

(* Exercise 3a *)
TEST_UNIT "mapi_lst_test1" = assert_true ((mapi_lst (+) [3;0;-1;-3]) = [3;1;1;0])
TEST_UNIT "mapi_lst_test2" = assert_true ((mapi_lst (-) [2;1;3]) = [2;0;1])
TEST_UNIT "mapi_lst_test3" = assert_true ((mapi_lst (+) []) = [])
TEST_UNIT "mapi_lst_test4" = assert_true ((mapi_lst (fun i a -> (float_of_int i) +. (float_of_int a)) [2;1;3]) = [2.;2.;5.])

(* Exercise 3b *)
TEST_UNIT "outline_test1" = assert_true ((outline ["point 1";"point 2";"point 3"]) = ["1. point 1";"2. point 2";"3. point 3"])
TEST_UNIT "outline_test2" = assert_true ((outline []) =  [])
TEST_UNIT "outline_test3" = assert_true ((outline [""]) =  ["1. "])

(* Exercise 4a *)
TEST_UNIT "scan_left_test1" = assert_true ((scan_left (+) 0 [1;2;3]) = [0;1;3;6])
TEST_UNIT "scan_left_test2" = assert_true ((scan_left (+) 0 []) = [0])
TEST_UNIT "scan_left_test3" = assert_true ((scan_left  (^) "swag" ["zar";"doz"]) = ["swag";"swagzar";"swagzardoz"])

TEST_UNIT "scan_right_test1" = assert_true ((scan_right (+) [1;2;3] 0) = [0;3;5;6])
TEST_UNIT "scan_right_test2" = assert_true ((scan_right (+) [] 0) = [0])
TEST_UNIT "scan_right_test3" = assert_true ((scan_right (^) ["zar";"doz"] "swag") = ["swag";"dozswag";"zardozswag"])

(* Exercise 4b *)
TEST_UNIT "fact_list_test1" = assert_true ((fact_list 3) = [1;2;6])
TEST_UNIT "fact_list_test1" = assert_true ((fact_list 1) = [1])
TEST_UNIT "fact_list_test1" = assert_true ((fact_list 4) = [1;2;6;24])

(* PROBLEM 3 *)
(* Exercise 1 *)
TEST_UNIT "show_test1" = assert_true ((show [[1;2;3];[42;41;40]]) = ())
TEST_UNIT "show_test2" = assert_true ((show [[5;3;2];[1;5;6]]) = ())

(* Exercise 2 *)
TEST_UNIT "insert_col_test1" = assert_true ((insert_col [[1;2;3];[42;41;40]] [6;7]) = [[1;2;3;6];[42;41;40;7]])
TEST_UNIT "insert_col_test2" = assert_raises (Some MatrixFailure) (insert_col [[1;2;3];[42;41;40]] [42;42;42])
TEST_UNIT "insert_col_test3" = assert_true ((insert_col [[1;2;3;4];[7;8;9;10]] [5;11]) = [[1;2;3;4;5];[7;8;9;10;11]])
TEST_UNIT "insert_col_test4" = assert_true ((insert_col [[];[]] [1;2]) = [[1];[2]])

(* Exercise 3 *)
TEST_UNIT "transpose_test1" = assert_true ((transpose [[1;2;3];[42;41;40]]) = [[1;42];[2;41];[3;40]])
TEST_UNIT "transpose_test2" = assert_true ((transpose []) = [])
TEST_UNIT "transpose_test3" = assert_true ((transpose [[];[]]) = [])
TEST_UNIT "transpose_test4" = assert_true ((transpose [[1];[2];[3];[4];[5]]) = [[1;2;3;4;5]])
TEST_UNIT "transpose_test5" = assert_true ((transpose [[1;2;3;4;5]]) = [[1];[2];[3];[4];[5]])
TEST_UNIT "transpose_test6" = assert_true ((transpose (transpose [[1];[2];[3];[4];[5]])) = [[1];[2];[3];[4];[5]])
TEST_UNIT "transpose_test7" = assert_true ((transpose [[1;1;1];[2;2;2];[3;3;3];[4;4;4]]) = [[1;2;3;4];[1;2;3;4];[1;2;3;4]])
TEST_UNIT "transpose_test8" = assert_true ((transpose (insert_col [[1;2;3;4];[7;8;9;10]] [5;11])) = [[1;7];[2;8];[3;9];[4;10];[5;11]])
TEST_UNIT "transpose_test9" = assert_raises (Some MatrixFailure) (transpose [[1;2];[3]])

(* Exercise 4 *)
TEST_UNIT "add_matrices_test1" = assert_true ((add_matrices [[1;2;3];[4;5;6]] [[42;42;42];[43;43;43]]) = [[43;44;45];[47;48;49]])
TEST_UNIT "add_matrices_test2" = assert_true ((add_matrices [] []) = [])
TEST_UNIT "add_matrices_test3" = assert_raises (Some MatrixFailure) (add_matrices [] [[1]])
TEST_UNIT "add_matrices_test4" = assert_raises (Some MatrixFailure) (add_matrices [[5];[2]] [[1]])
TEST_UNIT "add_matrices_test5" = assert_true ((add_matrices [[1];[2];[3];[4];[5]] [[9];[8];[7];[6];[5]]) = [[10];[10];[10];[10];[10]])

(* Exercise 5 *)
TEST_UNIT "multiply_matrices_test1" = assert_true ((multiply_matrices [[1;2;3];[4;5;6]] [[7;8];[9;10];[11;12]]) = [[58;64];[139;154]])
TEST_UNIT "multiply_matrices_test2" = assert_true ((multiply_matrices [] []) = [])
TEST_UNIT "multiply_matrices_test3" = assert_true ((multiply_matrices [[1;1;1];[2;2;2];[1;1;1]] [[2;2;2];[1;1;1];[2;2;2]]) = [[5;5;5];[10;10;10];[5;5;5]])
TEST_UNIT "multiply_matrices_test4" = assert_raises (Some MatrixFailure) (multiply_matrices [[1;2;3];[4;5;6]] [[7;8;9];[10;11;12]])
TEST_UNIT "multiply_matrices_test5" = assert_true ((multiply_matrices [[2];[5];[1]] [[4;2;6]]) = [[8;4;12];[20;10;30];[4;2;6]])

(* PROBLEM 4 *)
(* Exercise 1 *)
TEST_UNIT "count_wcs_test1" = assert_true ((count_wcs (TuplePat [WCPat;VarPat "whoa";UnitPat;WCPat;ConstPat 3;TuplePat [VarPat "well";WCPat];StructorPat ("hello",Some WCPat)])) = 4)
(* Exercise 2 *)
(* Exercise 3 *)
TEST_UNIT "all_answers_test1" = assert_true ((all_answers (fun x -> (Some ([x+1;x+2]))) []) = Some [])
TEST_UNIT "all_answers_test2" = assert_true ((all_answers (fun x -> (Some ([x+1;x+2]))) [1;2;3]) = Some [2;3;3;4;4;5])
(* Exercise 4 *)
(* Exercise 5 *)
(* Exercise 6 *)












