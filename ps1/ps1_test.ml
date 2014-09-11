open Assertions

(* Exercise 1 *)
let a = [1;2;3]
let b = [1;10;3]
let c = [5;4;3;2;1]
let d = []
let e = [10000]

TEST_UNIT "is_mon_ic_test1" = assert_true ((is_mon_ic a) = true)
TEST_UNIT "is_mon_ic_test2" = assert_true ((is_mon_ic b) = false)
TEST_UNIT "is_mon_ic_test3" = assert_true ((is_mon_ic c) = false)
TEST_UNIT "is_mon_ic_test4" = assert_true ((is_mon_ic d) = true)
TEST_UNIT "is_mon_ic_test5" = assert_true ((is_mon_ic e) = true)

