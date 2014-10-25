(* Charles Tark (cyt25) and Lillian Chen (qc53) *)
(* October 23, 2014 *)

open Assertions
open Iterator
open Mutability

let c1 = count_up_from 3 2
c1 ()
c1 ()
TEST_UNIT "count_up_from_test1" = assert_true (c1 () = 7)
let c2 = count_up_from 0 0
c2 ()
C2 ()
TEST_UNIT "count_up_from_test2" = assert_true (c2 () = 0)
let c3 = count_up_from 0 1
c3 ()
C3 ()
TEST_UNIT "count_up_from_test3" = assert_true (c3 () = 2)
let c4 = count_up_from 1 0
c4 ()
c4 ()
TEST_UNIT "count_up_from_test4" = assert_true (c4 () = 1)
let c5 = count_up_from 2 5
c5 ()
c5 ()
TEST_UNIT "count_up_from_test5" = assert_true (c5 () = 2+(3-1)*5)

TEST_UNIT "tabulate_test1" = assert_true ((tabulate (fun x -> x*x) 5) = [|0; 1; 4; 9; 16|])
TEST_UNIT "tabulate_test2" = assert_true ((tabulate (fun x -> string_of_int x) 3) = [|"0"; "1"; "2"|])
TEST_UNIT "tabulate_test3" = assert_true ((tabulate (fun x -> x) 0) = [||])

TEST_UNIT "fold_left_imp_test1" = assert_true (fold_left_imp (+) 0 [1;2;3] = 6)
TEST_UNIT "fold_left_imp_test2" = assert_true (fold_left_imp (fun x y -> x - y) 10 [1;2;3] = 4)
TEST_UNIT "fold_left_imp_test3" = assert_true (fold_left_imp (@) [] [[1;2;3];[4;5];[6]] = [1;2;3;4;5;6])

let lst2 = [1;1;1]
TEST_UNIT "zardoz_test" = assert_true ((List.map zardoz (List.rev lst) = List.rev (List.map zardoz lst)) = false)
TEST_UNIT "zardoz_test" = assert_true ((List.map zardoz (List.rev lst2) = List.rev (List.map zardoz lst2)) = false)

let l1 = ListIterator.create [1;2;3;4;5]
TEST_UNIT "listiterator_has_next_test1" = assert_true ((ListIterator.has_next l1) = true)
TEST_UNIT "listiterator_next_test1" = assert_true ((ListIterator.next l1) = 1)
TEST_UNIT "listiterator_has_next_test2" = assert_true ((ListIterator.has_next l1) = true)
TEST_UNIT "listiterator_next_test2" = assert_true ((ListIterator.next l1) = 2)
TEST_UNIT "listiterator_has_next_test3" = assert_true ((ListIterator.has_next l1) = true)
TEST_UNIT "listiterator_next_test3" = assert_true ((ListIterator.next l1) = 3)
TEST_UNIT "listiterator_has_next_test4" = assert_true ((ListIterator.has_next l1) = true)
TEST_UNIT "listiterator_next_test4" = assert_true ((ListIterator.next l1) = 4)
TEST_UNIT "listiterator_has_next_test5" = assert_true ((ListIterator.has_next l1) = true)
TEST_UNIT "listiterator_next_test5" = assert_true ((ListIterator.next l1) = 5)
TEST_UNIT "listiterator_has_next_test6" = assert_true ((ListIterator.has_next l1) = false)
TEST_UNIT "listiterator_next_test6" = assert_raises (Some (ListIterator.NoResult)) (ListIterator.next) (l1)

let t1 = InorderTreeIterator.create (Node(2,Node(1,Leaf,Leaf),Leaf))
TEST_UNIT "inordertreeiterator_has_next_test1" = assert_true ((InorderTreeIterator.has_next t1) = true)
TEST_UNIT "inordertreeiterator_next_test1" = assert_true ((InorderTreeIterator.next t1) = 1)
TEST_UNIT "inordertreeiterator_has_next_test2" = assert_true ((InorderTreeIterator.has_next t1) = true)
TEST_UNIT "inordertreeiterator_next_test2" = assert_true ((InorderTreeIterator.next t1) = 2)
TEST_UNIT "inordertreeiterator_has_next_test3" = assert_true ((InorderTreeIterator.has_next t1) = false)
TEST_UNIT "inordertreeiterator_next_test3" = assert_raises (Some (InorderTreeIterator.NoResult)) (InorderTreeIterator.next) (t1)

let t2 = InorderTreeIterator.create (Node(1,Leaf,Node(2,Leaf,Leaf)))
TEST_UNIT "inordertreeiterator_has_next_test1" = assert_true ((InorderTreeIterator.has_next t2) = true)
TEST_UNIT "inordertreeiterator_next_test1" = assert_true ((InorderTreeIterator.next t2) = 1)
TEST_UNIT "inordertreeiterator_has_next_test2" = assert_true ((InorderTreeIterator.has_next t2) = true)
TEST_UNIT "inordertreeiterator_next_test2" = assert_true ((InorderTreeIterator.next t2) = 2)
TEST_UNIT "inordertreeiterator_has_next_test3" = assert_true ((InorderTreeIterator.has_next t2) = false)
TEST_UNIT "inordertreeiterator_next_test3" = assert_raises (Some (InorderTreeIterator.NoResult)) (InorderTreeIterator.next) (t2)

let t3 = InorderTreeIterator.create (Node(4,Node(2,Node(1,Leaf,Leaf),Node(3,Leaf,Leaf)),Node(6,Node(5,Leaf,Leaf),Node(7,Leaf,Leaf))))
TEST_UNIT "inordertreeiterator_has_next_test4" = assert_true ((InorderTreeIterator.has_next t3) = true)
TEST_UNIT "inordertreeiterator_next_test4" = assert_true ((InorderTreeIterator.next t3) = 1)
TEST_UNIT "inordertreeiterator_has_next_test5" = assert_true ((InorderTreeIterator.has_next t3) = true)
TEST_UNIT "inordertreeiterator_next_test5" = assert_true ((InorderTreeIterator.next t3) = 2)
TEST_UNIT "inordertreeiterator_has_next_test6" = assert_true ((InorderTreeIterator.has_next t3) = true)
TEST_UNIT "inordertreeiterator_next_test6" = assert_true ((InorderTreeIterator.next t3) = 3)
TEST_UNIT "inordertreeiterator_has_next_test7" = assert_true ((InorderTreeIterator.has_next t3) = true)
TEST_UNIT "inordertreeiterator_next_test7" = assert_true ((InorderTreeIterator.next t3) = 4)
TEST_UNIT "inordertreeiterator_has_next_test8" = assert_true ((InorderTreeIterator.has_next t3) = true)
TEST_UNIT "inordertreeiterator_next_test8" = assert_true ((InorderTreeIterator.next t3) = 5)
TEST_UNIT "inordertreeiterator_has_next_test9" = assert_true ((InorderTreeIterator.has_next t3) = true)
TEST_UNIT "inordertreeiterator_next_test9" = assert_true ((InorderTreeIterator.next t3) = 6)
TEST_UNIT "inordertreeiterator_has_next_test10" = assert_true ((InorderTreeIterator.has_next t3) = true)
TEST_UNIT "inordertreeiterator_next_test10" = assert_true ((InorderTreeIterator.next t3) = 7)
TEST_UNIT "inordertreeiterator_has_next_test11" = assert_true ((InorderTreeIterator.has_next t3) = false)
TEST_UNIT "inordertreeiterator_next_test11" = assert_raises (Some (InorderTreeIterator.NoResult)) (InorderTreeIterator.next) (t3)

module TakeListIterator = TakeIterator(ListIterator)
let l2 = ListIterator.create [10;8;6;4]
let l3 = TakeListIterator.create 3 l2
TEST_UNIT "takeiterator_has_next_test1" = assert_true ((TakeListIterator.has_next l3) = true)
TEST_UNIT "takeiterator_next_test1" = assert_true ((TakeListIterator.next l3) = 10)
TEST_UNIT "takeiterator_has_next_test2" = assert_true ((TakeListIterator.has_next l3) = true)
TEST_UNIT "takeiterator_next_test2" = assert_true ((TakeListIterator.next l3) = 8)
TEST_UNIT "takeiterator_has_next_test3" = assert_true ((TakeListIterator.has_next l3) = true)
TEST_UNIT "takeiterator_next_test3" = assert_true ((TakeListIterator.next l3) = 6)
TEST_UNIT "takeiterator_has_next_test4" = assert_true ((TakeListIterator.has_next l3) = false)
TEST_UNIT "takeiterator_next_test4" = assert_raises (Some (TakeListIterator.NoResult)) (TakeListIterator.next) (l3)

module TakeTreeIterator = TakeIterator(InorderTreeIterator)
let t3 = InorderTreeIterator.create (Node(2,Node(1,Leaf,Leaf),Leaf))
let t4 = TakeTreeIterator.create 0 t3
TEST_UNIT "takeiterator_has_next_test5" = assert_true ((TakeTreeIterator.has_next t4) = false)
TEST_UNIT "takeiterator_next_test5" = assert_raises (Some (TakeTreeIterator.NoResult)) (TakeTreeIterator.next) (t4)

let t5 = InorderTreeIterator.create (Node(4,Node(2,Node(1,Leaf,Leaf),Node(3,Leaf,Leaf)),Node(6,Node(5,Leaf,Leaf),Node(7,Leaf,Leaf))))
let t6 = TakeTreeIterator.create 3 t5
TEST_UNIT "takeiterator_has_next_test6" = assert_true ((TakeTreeIterator.has_next t6) = true)
TEST_UNIT "takeiterator_next_test6" = assert_true ((TakeTreeIterator.next t6) = 4)
TEST_UNIT "takeiterator_has_next_test7" = assert_true ((TakeTreeIterator.has_next t6) = true)
TEST_UNIT "takeiterator_next_test7" = assert_true ((TakeTreeIterator.next t6) = 5)
TEST_UNIT "takeiterator_has_next_test8" = assert_true ((TakeTreeIterator.has_next t6) = true)
TEST_UNIT "takeiterator_next_test8" = assert_true ((TakeTreeIterator.next t6) = 6)
TEST_UNIT "takeiterator_has_next_test8" = assert_true ((TakeTreeIterator.has_next t6) = true)
TEST_UNIT "takeiterator_next_test8" = assert_true ((TakeTreeIterator.next t6) = 7)
TEST_UNIT "takeiterator_has_next_test9" = assert_true ((TakeTreeIterator.has_next t6) = false)
TEST_UNIT "takeiterator_next_test9" = assert_raises (Some (TakeTreeIterator.NoResult)) (TakeTreeIterator.next) (t6)

module ListIteratorUtilsFn = IteratorUtilsFn(ListIterator)
let l4 = ListIterator.create [1;2;3;4;5;6;7;8;9;10]
let u = ListIteratorUtilsFn.advance 5 l4
TEST_UNIT "iteratorutilsfn_advance_test1" = assert_true ((ListIterator.next l4) = 6)
let u = ListIteratorUtilsFn.advance 3 l4
TEST_UNIT "iteratorutilsfn_advance_test2" = assert_true ((ListIterator.next l4) = 10)
TEST_UNIT "iteratorutilsfn_advance_test3" = assert_true ((ListIterator.has_next l4) = false)
TEST_UNIT "iteratorutilsfn_advance_test4" = assert_raises (Some (ListIterator.NoResult)) (ListIterator.next) (l4)

module TreeIteratorUtilsFn = IteratorUtilsFn(InorderTreeIterator)
let t7 = InorderTreeIterator.create (Node(4,Node(2,Node(1,Leaf,Leaf),Node(3,Leaf,Leaf)),Node(6,Node(5,Leaf,Leaf),Node(7,Leaf,Leaf))))
let u = TreeIteratorUtilsFn.advance 4 t7
TEST_UNIT "iteratorutilsfn_advance_test4" = assert_true ((InorderTreeIterator.next t7) = 5)
TEST_UNIT "iteratorutilsfn_advance_test5" = assert_true ((InorderTreeIterator.next t7) = 6)
let u = TreeIteratorUtilsFn.advance 0 t7
TEST_UNIT "iteratorutilsfn_advance_test6" = assert_true ((InorderTreeIterator.next t7) = 7)
TEST_UNIT "iteratorutilsfn_advance_test7" = assert_true ((InorderTreeIterator.has_next t7) = false)
TEST_UNIT "iteratorutilsfn_advance_test8" = assert_raises (Some (InorderTreeIterator.NoResult)) (InorderTreeIterator.next) (t7)

let l5 = ListIterator.create [1;2;3;4;5;6;7;8;9;10]
TEST_UNIT "iteratorutilsfn_fold_test1" = assert_true ((ListIteratorUtilsFn.fold (+) 0 l5) = 55)
TEST_UNIT "iteratorutilsfn_fold_test2" = assert_true ((ListIterator.has_next l5) = false)
TEST_UNIT "iteratorutilsfn_fold_test3" = assert_raises (Some (ListIterator.NoResult)) (ListIterator.next) (l5)

let l6 = ListIterator.create [1;2;3;4;5;6;7;8;9;10]
TEST_UNIT "iteratorutilsfn_fold_test3" = assert_true ((ListIteratorUtilsFn.fold (-) 100 l6) = 45)
TEST_UNIT "iteratorutilsfn_fold_test4" = assert_true ((ListIterator.has_next l6) = false)
TEST_UNIT "iteratorutilsfn_fold_test5" = assert_raises (Some (ListIterator.NoResult)) (ListIterator.next) (l6)

let l7 = ListIterator.create ["the ";"fault ";"in ";"our ";"code"]
TEST_UNIT "iteratorutilsfn_fold_test5" = assert_true ((ListIteratorUtilsFn.fold (fun a x -> a ^ x) "" l7) = "the fault in our code")
TEST_UNIT "iteratorutilsfn_fold_test6" = assert_true ((ListIterator.has_next l7) = false)
TEST_UNIT "iteratorutilsfn_fold_test7" = assert_raises (Some (ListIterator.NoResult)) (ListIterator.next) (l7)

let t8 = InorderTreeIterator.create (Node(4,Node(2,Node(1,Leaf,Leaf),Node(3,Leaf,Leaf)),Node(6,Node(5,Leaf,Leaf),Node(7,Leaf,Leaf))))
TEST_UNIT "iteratorutilsfn_fold_test7" = assert_true ((TreeIteratorUtilsFn.fold (+) 0 t8) = 28)
TEST_UNIT "iteratorutilsfn_fold_test8" = assert_true ((InorderTreeIterator.has_next t8) = false)
TEST_UNIT "iteratorutilsfn_fold_test9" = assert_raises (Some (InorderTreeIterator.NoResult)) (InorderTreeIterator.next) (t8)

module ListRangeIterator = RangeIterator(ListIterator)
let l8 = ListIterator.create [1;2;3;4;5;6;7;8;9;10]
let l9 = ListRangeIterator.create 4 8 l8
TEST_UNIT "rangeiterator_has_next_test1" = assert_true ((ListRangeIterator.has_next l9) = true)
TEST_UNIT "rangeiterator_next_test1" = assert_true ((ListRangeIterator.next l9) = 4)
TEST_UNIT "rangeiterator_has_next_test2" = assert_true ((ListRangeIterator.has_next l9) = true)
TEST_UNIT "rangeiterator_next_test2" = assert_true ((ListRangeIterator.next l9) = 5)
TEST_UNIT "rangeiterator_has_next_test3" = assert_true ((ListRangeIterator.has_next l9) = true)
TEST_UNIT "rangeiterator_next_test3" = assert_true ((ListRangeIterator.next l9) = 6)
TEST_UNIT "rangeiterator_has_next_test4" = assert_true ((ListRangeIterator.has_next l9) = true)
TEST_UNIT "rangeiterator_next_test4" = assert_true ((ListRangeIterator.next l9) = 7)
TEST_UNIT "rangeiterator_has_next_test5" = assert_true ((ListRangeIterator.has_next l9) = true)
TEST_UNIT "rangeiterator_next_test5" = assert_true ((ListRangeIterator.next l9) = 8)
TEST_UNIT "rangeiterator_has_next_test6" = assert_true ((ListRangeIterator.has_next l9) = false)
TEST_UNIT "rangeiterator_next_test6" = assert_raises (Some (ListRangeIterator.NoResult)) (ListRangeIterator.next) (l9)

module TreeRangeIterator = RangeIterator(InorderTreeIterator)
let t9 = InorderTreeIterator.create (Node(4,Node(2,Node(1,Leaf,Leaf),Node(3,Leaf,Leaf)),Node(6,Node(5,Leaf,Leaf),Node(7,Leaf,Leaf))))
let t10 = TreeRangeIterator.create 3 7 t9
TEST_UNIT "rangeiterator_has_next_test7" = assert_true ((TreeRangeIterator.has_next t10) = true)
TEST_UNIT "rangeiterator_next_test7" = assert_true ((TreeRangeIterator.next t10) = 3)
TEST_UNIT "rangeiterator_has_next_test8" = assert_true ((TreeRangeIterator.has_next t10) = true)
TEST_UNIT "rangeiterator_next_test8" = assert_true ((TreeRangeIterator.next t10) = 4)
TEST_UNIT "rangeiterator_has_next_test9" = assert_true ((TreeRangeIterator.has_next t10) = true)
TEST_UNIT "rangeiterator_next_test9" = assert_true ((TreeRangeIterator.next t10) = 5)
TEST_UNIT "rangeiterator_has_next_test10" = assert_true ((TreeRangeIterator.has_next t10) = true)
TEST_UNIT "rangeiterator_next_test10" = assert_true ((TreeRangeIterator.next t10) = 6)
TEST_UNIT "rangeiterator_has_next_test11" = assert_true ((TreeRangeIterator.has_next t10) = true)
TEST_UNIT "rangeiterator_next_test11" = assert_true ((TreeRangeIterator.next t10) = 7)
TEST_UNIT "rangeiterator_has_next_test12" = assert_true ((TreeRangeIterator.has_next t10) = false)
TEST_UNIT "rangeiterator_next_test12" = assert_raises (Some (TreeRangeIterator.NoResult)) (TreeRangeIterator.next) (t10)
