(* Charles Tark (cyt25) and Lillian Chen (qc53) *)
(* October 23, 2014 *)

open Assertions
open Mutability

let c1 = count_up_from 3 2
let b1 = c1 ()
let b1 = c1 ()
TEST_UNIT "count_up_from_test1" = assert_true (c1 () = 7)
let c2 = count_up_from 0 0
let b2 = c2 ()
let b2 = c2 ()
TEST_UNIT "count_up_from_test2" = assert_true (c2 () = 0)
let c3 = count_up_from 0 1
let b3 = c3 ()
let b3 = c3 ()
TEST_UNIT "count_up_from_test3" = assert_true (c3 () = 2)
let c4 = count_up_from 1 0
let b4 = c4 ()
let b4 = c4 ()
TEST_UNIT "count_up_from_test4" = assert_true (c4 () = 1)
let c5 = count_up_from 2 5
let b5 = c5 ()
let b5 = c5 ()
TEST_UNIT "count_up_from_test5" = assert_true (c5 () = 2+(3-1)*5)

TEST_UNIT "tabulate_test1" = assert_true ((tabulate (fun x -> x*x) 5) = [|0; 1; 4; 9; 16|])
TEST_UNIT "tabulate_test2" = assert_true ((tabulate (fun x -> string_of_int x) 3) = [|"0"; "1"; "2"|])
TEST_UNIT "tabulate_test3" = assert_true ((tabulate (fun x -> x) 0) = [||])

TEST_UNIT "fold_left_imp_test1" = assert_true (fold_left_imp (+) 0 [1;2;3] = 6)
TEST_UNIT "fold_left_imp_test2" = assert_true (fold_left_imp (fun x y -> x - y) 10 [1;2;3] = 4)
TEST_UNIT "fold_left_imp_test3" = assert_true (fold_left_imp (@) [] [[1;2;3];[4;5];[6]] = [1;2;3;4;5;6])

let lst2 = [1;1;1]
TEST_UNIT "zardoz_test" = assert_true ((List.map zardoz (List.rev lst) = List.rev (List.map zardoz lst)) = false)
TEST_UNIT "zardoz_test" = assert_true ((List.map zardoz (List.rev lst) = List.rev (List.map zardoz lst)) = false)
