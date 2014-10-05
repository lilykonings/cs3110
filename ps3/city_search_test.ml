(* Charles Tark (cyt25) and Lillian Chen (qc53) *)
(* October 2, 2014 *)

open Assertions
open Parser
open Quadtree
open City_search

TEST_UNIT "load_city_data_test1" = assert_true ((load_city_data "") = Leaf (((-90., -180.), (90., 180.)), []))
TEST_UNIT "load_city_data_test2" = assert_true ((load_city_data "test_cases.csv") =
	Node (((-90., -180.), (90., 180.)),
		Leaf (((0., 0.), (90., 180.)), [((65.2904242, 29.042249), "World")]),            
	 Leaf (((-90., 0.), (0., 180.)), []),
	 Leaf (((-90., -180.), (0., 0.)), [((-19.9024224, -76.2428242), "Hello")]),
	 Leaf (((0., -180.), (90., 0.)), [((29.4920242, -22.4924404), "OCaml")]))
)

let test = load_city_data "test_cases.csv"
TEST_UNIT "city_search_test1" = assert_true ((city_search test (((-90.),(-180.)),(90.,180.))) = ["OCaml"; "Hello"; "World"])
TEST_UNIT "city_search_test2" = assert_true ((city_search test ((0.,0.),(90.,180.))) = ["World"])
TEST_UNIT "city_search_test2" = assert_true ((city_search test ((0.,0.),(2.,4.))) = [])