
open Essay ;;
open OUnit2;;

(* unit tests *)

let add_test1 _test_ctxt =
  assert_equal 9 (add 5 4) ;;

(* it tests the mult_int, 3 times 2 is equal to 6 *)
let mul_int_utest _text_ctxt =
  assert_equal 6 (mul_int 3 2) ;;

(* it tests the div_float, 12.0 divided by 4.0  *)
let div_float_utest _text_ctxt =
  assert_equal 3.0 (div_float 12.0 4.0) ;;

(* tests if string list counts the elements*)
let count_string_list_utest _text_ctxt =
  assert_equal 3 (count ["asdf"; "sdfsdf"; "sdfasdf"]) ;;

(* tests if it returns true *)
let is_good_weather_utest _text_ctxt =
  assert_equal true (is_good_weather "sunny") ;;

(* tests if it returns true *)
let is_good_weather_utest2 _text_ctxt =
  assert_equal false (is_good_weather "sdggerg") ;;

let get_head_utest _text_ctxt =
  assert_equal 4 (get_head [4;6;1]) ;;

(* list of unit tests *)
let unit_tests =
  [ "add_test1">::add_test1
  ; "mul_int_utest">::mul_int_utest
  ; "div_float_utest">::div_float_utest
  ; "count_string_list_utest">::count_string_list_utest
  ; "is_good_weather_utest">::is_good_weather_utest
  ; "is_good_weather_utest2">::is_good_weather_utest2
  ; "get_head_utest">::get_head_utest
  ];;

(* property based tests *)

(* integer sequences of random natural numbers with length 5 *)
let int_list_5_gen =
  QCheck.Gen.(list_size (return 5) nat) ;;

(* float sequences of natural numbers with length 15 *)
let float_list_7_gen =
  QCheck.Gen.(list_size (return 7) float) ;;

(* gets head equal to zero from a zero integer list *)
let get_head_ptest =
  QCheck.Test.make ~make: "get_head_int_list" ~count: 100
    QCheck.(make float_list_7_gen)
    (fun list -> get_head list = 0 )

(* counts integer sequences *)
let count_int_list_ptest =
  QCheck.Test.make ~name:"count_int_list" ~count:100
    QCheck.(make int_list_5_gen)
    ( fun list -> count list = 5 )

(* counts integer sequences *)
let count_float_list_ptest =
  QCheck.Test.make ~name:"count_float_list" ~count:100
    QCheck.(make float_list_7_gen)
    ( fun list -> count list = 7 )

(* checks if 0.0 is divided by a random number should return 0 *)
let div_float_ptest =
  QCheck.Test.make ~name:"div_float" ~count: 1000
    QCheck.(make Gen.float)
      (fun x -> div_float 0.0 x = 0.0 ) ;;

(* check if a random number is multiplied by zero  should return 0*)
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
    ; div_float_ptest
    ; count_int_list_ptest
    ; count_float_list_ptest
    ; get_head_ptest
    ];;

(* run the unit and property based tests *)
let () =
  run_test_tt_main
    ("sequence_arithmetic_tests">:::
       (List.append unit_tests property_tests));;
