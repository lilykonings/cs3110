(* Charles Tark (cyt25) and Lillian Chen (qc53) *)
(* October 2, 2014 *)

open Assertions
open Quadtree

let q1_1 = Leaf (((3.,3.),(5.,5.)),[])
let q1_2 = Leaf (((1.,3.),(3.,5.)),[])
let q1_3 = Leaf (((1.,1.),(3.,3.)),[])
let q1_4 = Leaf (((3.,1.),(5.,3.)),[])
let q1 = Node (((1.,1.),(5.,5.)), q1_1, q1_2, q1_3, q1_4)

TEST_UNIT "new_tree_test1" = assert_true ((new_tree ((3.,3.),(5.,5.))) = q1_1)
TEST_UNIT "new_tree_test2" = assert_true ((new_tree ((1.,3.),(3.,5.))) = q1_2)
TEST_UNIT "new_tree_test3" = assert_true ((new_tree ((1.,1.),(3.,3.))) = q1_3)
TEST_UNIT "new_tree_test4" = assert_true ((new_tree ((3.,1.),(5.,3.))) = q1_4)

let q2_1_3_1 = Leaf (((3.5,3.5),(4.,4.)), [((3.65,3.7),2)])
let q2_1_3_2 = Leaf (((3.,3.5),(3.5,4.)), [])
let q2_1_3_3 = Leaf (((3.,3.),(3.5,3.5)), [((3.4,3.3),1)])
let q2_1_3_4 = Leaf (((3.5,3.),(4.,3.5)), [])
let q2_1_1 = Leaf (((4.,4.),(5.,5.)), [])
let q2_1_2 = Leaf (((3.,4.),(4.,5.)), [])
let q2_1_3 = Node (((3.,3.),(4.,4.)), q2_1_3_1, q2_1_3_2, q2_1_3_3, q2_1_3_4)
let q2_1_4 = Leaf (((4.,3.),(5.,4.)), [])
let q2_1 = Node (((3.,3.),(5.,5.)), q2_1_1, q2_1_2, q2_1_3, q2_1_4)
let q2_2 = Leaf (((1.,3.),(3.,5.)),[((1.5,3.8),3)])
let q2_3 = Leaf (((1.,1.),(3.,3.)),[])
let q2_4 = Leaf (((3.,1.),(5.,3.)),[((4.3,1.5),4)])
let q2 = Node (((1.,1.),(5.,5.)),q2_1, q2_2, q2_3, q2_4)

TEST_UNIT "find_quad_test1" = assert_true ((find_quad ((3.,4.),(4.,5.)) (1.5,3.8)) = 0)
TEST_UNIT "find_quad_test2" = assert_true ((find_quad ((1.,3.),(3.,5.)) (1.5,3.8)) = 3)
TEST_UNIT "find_quad_test3" = assert_true ((find_quad ((1.,1.),(5.,5.)) (4.3,1.5)) = 4)
TEST_UNIT "find_quad_test4" = assert_true ((find_quad ((3.,4.),(4.,5.)) (3.5,3.8)) = 0)
TEST_UNIT "find_quad_test5" = assert_true ((find_quad ((3.,4.),(4.,5.)) (1.5,100.)) = 0)
TEST_UNIT "find_quad_test6" = assert_true ((find_quad ((0.,0.),(100.,100.)) (53.,74.)) = 1)
TEST_UNIT "find_quad_test7" = assert_true ((find_quad ((0.,0.),(100.,100.)) (25.,95.)) = 2)
TEST_UNIT "insert_test1" = assert_raises (Some OutOfBounds) (insert q2 (0.,0.)) (42)
TEST_UNIT "insert_test2" = assert_true ((insert (Leaf (((3.5,3.5),(4.,4.)), [])) (3.65,3.7) 2) = q2_1_3_1)
TEST_UNIT "insert_test3" = assert_true ((insert (Leaf (((3.,3.),(3.5,3.5)), [])) (3.4,3.3) 1) = q2_1_3_3)
TEST_UNIT "insert_test4" = assert_true ((insert (Leaf (((3.,3.),(5.,5.)), [((3.4,3.3),1)])) (3.65,3.7) 2) = q2_1)
TEST_UNIT "insert_test5" = assert_true ((insert q2_1 (3.9,3.5) 5) =
	Node (((3., 3.), (5., 5.)),
		Leaf (((4., 4.), (5., 5.)), []),
		Leaf (((3., 4.), (4., 5.)), []),
		Node (((3., 3.), (4., 4.)),
			Leaf (((3.5, 3.5), (4., 4.)), [((3.65, 3.7), 2)]),
			Leaf (((3., 3.5), (3.5, 4.)), []),
			Leaf (((3., 3.), (3.5, 3.5)), [((3.4, 3.3), 1)]),
			Leaf (((3.5, 3.), (4., 3.5)), [((3.9, 3.5), 5)])),
		Leaf (((4., 3.), (5., 4.)), [])))
TEST_UNIT "insert_test5" = assert_true ((insert q2_1_3_1 (3.9,3.5) 5) =
	Node (((3.5, 3.5), (4., 4.)),
		Leaf (((3.75, 3.75), (4., 4.)), []),
		Leaf (((3.5, 3.75), (3.75, 4.)), []),                                            
		Leaf (((3.5, 3.5), (3.75, 3.75)), [((3.65, 3.7), 2)]),
		Leaf (((3.75, 3.5), (4., 3.75)), [((3.9, 3.5), 5)])))

TEST_UNIT "fold_quad_test1" = assert_true ((fold_quad (fun a c -> (snd c) + a) 0 q2) = 10)
TEST_UNIT "fold_quad_test2" = assert_true ((fold_quad (fun a c -> (snd c) * a) 1 q2) = 24)
TEST_UNIT "fold_quad_test3" = assert_true ((fold_quad (fun a c -> (snd c) + a) 0 q1) = 0)
TEST_UNIT "fold_quad_test4" = assert_true ((fold_quad (fun a c -> (snd c) * a) 1 q2_1) = 2)

TEST_UNIT "fold_region_test1" = assert_true ((fold_region (fun a c -> (snd c) + a) 0 q2 ((1.,1.),(5.,5.))) = 10)
TEST_UNIT "fold_region_test2" = assert_true ((fold_region (fun a c -> (snd c) + a) 0 q2 ((3.,3.),(5.,5.))) = 3)
TEST_UNIT "fold_region_test2" = assert_true ((fold_region (fun a c -> (snd c) + a) 0 q2 ((10.,10.),(15.,15.))) = 0)
TEST_UNIT "fold_region_test4" = assert_true ((fold_region (fun a c -> (snd c) * a) 1 q2 ((1.,1.),(5.,5.))) = 24)
TEST_UNIT "fold_region_test5" = assert_true ((fold_region (fun a c -> (snd c) * a) 1 q2 ((1.,3.),(3.,5.))) = 3)
TEST_UNIT "fold_region_test6" = assert_true ((fold_region (fun a c -> (snd c) * a) 1 q2 ((3.,1.),(5.,5.))) = 8)
TEST_UNIT "fold_region_test4" = assert_true ((fold_region (fun a c -> (snd c) * a) 1 q2_1 ((3.5,0.),(4.5,8.))) = 2)