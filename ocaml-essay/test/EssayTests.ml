
open Essay ;;
open OUnit2;;

(* unit tests *)

let add_test1 _test_ctxt =
  assert_equal 9 (add 5 4) ;;

(* it tests if 3 times 2 is equal to 6 *)
let mul_int_utest _text_ctxt =
  assert_equal 6 (mul_int 3 2) ;;

(* list of unit tests *)
let unit_tests =
  [ "add_test1">::add_test1
  ; "mul_int_utest">::mul_int_utest
  ];;

(* property based tests *)

(* check if a random number is multiplied by zero *)
let mul_int_ptest =
  QCheck.Test.make ~name:"mul_int" ~count:1000
    QCheck.(make Gen.nat )
      (fun x -> mul_int x 0 = 0 ) ;;

let add_zero =
  QCheck.Test.make ~name:"seqmult_zeros" ~count:1000
    QCheck.(make Gen.nat)
    (fun x ->
      add x 0 = x
      && add 0 x = x);;

(* list of all property tests *)                  
let property_tests =
  List.map QCheck_ounit.to_ounit2_test
    [ add_zero
    ; mul_int_ptest
    ];;

(* run the unit and property based tests *)
let () =
  run_test_tt_main
    ("sequence_arithmetic_tests">:::
       (List.append unit_tests property_tests));;
