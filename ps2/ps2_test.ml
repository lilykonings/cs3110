(* Lillian Chen (qc53) and Charles Tark (cyt25) *)
(* September 18, 2014 *)

open Assertions
open Ps2

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
let f i a =
	(float_of_int i) +. (float_of_int a)

TEST_UNIT "mapi_lst_test1" = assert_true ((mapi_lst (+) [3;0;-1;-3]) = [3;1;1;0])
TEST_UNIT "mapi_lst_test2" = assert_true ((mapi_lst (-) [2;1;3]) = [2;0;1])
TEST_UNIT "mapi_lst_test3" = assert_true ((mapi_lst (+) []) = [])
TEST_UNIT "mapi_lst_test4" = assert_true ((mapi_lst f [2;1;3])) = [2.;2.;5.])

(* Exercise 3b *)
TEST_UNIT "outline_test1" = assert_true ((outline ["point 1"; "point 2"; "point 3"]) =  ["1. point 1"; "2. point 2"; "3. point 3"])
TEST_UNIT "outline_test2" = assert_true ((outline []) =  [])
TEST_UNIT "outline_test3" = assert_true ((outline [""]) =  ["1. "])

(* Exercise 4a *)
TEST_UNIT "scan_left_test1" = assert_true ((scan_left (+) 0 [1; 2; 3]) = [0; 1; 3; 6])
TEST_UNIT "scan_left_test2" = assert_true ((scan_left (+) 0 []) = [0])
TEST_UNIT "scan_left_test3" = assert_true ((scan_left  (^) "swag" ["zar"; "doz"] = ["swag"; "swagzar"; "swagzardoz"])

TEST_UNIT "scan_right_test1" = assert_true ((scan_right (+) [1; 2; 3] 0) = [0; 3; 5; 6])
TEST_UNIT "scan_right_test2" = assert_true ((scan_right (+) [] 0) = [0])
TEST_UNIT "scan_right_test3" = assert_true ((scan_right (^) ["zar"; "doz"] "swag"= ["swag"; "dozswag"; "zardozswag"])

(* Exercise 4b *)
TEST_UNIT "fact_list_test1" = assert_true ((fact_list 3 = [1;2;6])
TEST_UNIT "fact_list_test1" = assert_true ((fact_list 1 = [1])
TEST_UNIT "fact_list_test1" = assert_true ((fact_list 4 = [1;2;6;24])
