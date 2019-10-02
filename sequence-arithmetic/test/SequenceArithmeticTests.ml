open SequenceArithmetic ;;
open OUnit2;;

(* unit tests *)

(* test seqadd on two empty lists *)
let seqadd_test1 _test_ctxt =
  assert_equal ([]) (seqadd [] []);;

(* test seqadd on two non-empty lists *)
let seqadd_test2 _test_ctxt =
  assert_equal [7;5;6] (seqadd [2;3;5] [5;2;1]);;

(* another test of seqadd on two non-empty lists *)
let seqadd_test3 _test_ctxt =
  (* TODO *)
  assert_equal [6;10;15] (seqadd [3;5;10] [3;5;5]) ;;

(* another test of seqadd on two non-empty lists *)
let seqadd_test4 _test_ctxt =
  (* TODO *)
  assert_equal [8;9;10] (seqadd [1;8;5] [7;1;5]) ;;

(* test of seqmult on two empty lists *)
let seqmult_test1 _test_ctxt =
  (* TODO *)
  assert_equal [8;12;24] (seqmult [4;2;12] [2;6;2]) ;;

(* test seqmult on two non-empty lists *)
let seqmult_test2 _test_ctxt =
  (* TODO *)
  assert_equal [9;6;12] (seqmult [3;2;4] [3;3;3]) ;;

(* test seqmult on two non-empty lists *)
let seqmult_test3 _test_ctxt =
  (* TODO *)
  assert_equal [16;18;12] (seqmult [4;2;6] [4;9;2]) ;;

(* test seqmult on two non-empty lists *)
let seqmult_test4 _test_ctxt =
  (* TODO *)
  assert_equal [2;3;6] (seqmult [1;3;3] [2;1;2]) ;;

(* list of unit tests *)
let unit_tests =
  [ "seqadd_test1">::seqadd_test1
  ; "seqadd_test2">::seqadd_test2
  ; "seqadd_test3">::seqadd_test3
  ; "seqadd_test4">::seqadd_test4
  ; "seqmult_test1">::seqmult_test1
  ; "seqmult_test2">::seqmult_test2
  ; "seqmult_test3">::seqmult_test3
  ; "seqmult_test4">::seqmult_test4
  ];;

(* property based tests *)

(* integer sequence generator of 100 elements *)
let integer_sequence_gen =
  QCheck.Gen.(list_size (return 100) nat);;

(* zeros sequence generator of 100 elements *)
let zeros_gen =
  QCheck.Gen.(list_size (return 100) (return 0));;

(* pretty printer to show counter examples of sequence property tests *)
let show_sequence (seq1,seq2) =
  String.concat "" (List.map string_of_int seq1) ^ "\n"
  ^ String.concat "" (List.map string_of_int seq2) ;;

(* adding a sequence to an empty sequence should return an empty 
 * e.g. seqadd [1;2;3] [] = [] *)
let seqadd_empty1 =
  QCheck.Test.make ~name:"seqadd_empty1" ~count:10000
    QCheck.(make integer_sequence_gen)
    (fun seq ->
      seqadd seq [] = []);;

(* adding a sequence to an empty sequence should return an empty 
 * e.g. seqadd [] [1;2;3] = [] *)
let seqadd_empty2 =
  QCheck.Test.make ~name:"seqadd_empty2" ~count:10000
    QCheck.(make integer_sequence_gen)
    (fun seq ->
      seqadd [] seq = []);;

(* adding 0 to each element in a sequence result in the same sequence,
   and hence the input sequence should be the same as the output sequence 
   e.g. seqadd [1;2;3] [0;0;0] => [1;2;3]
*)
let seqadd_zeros =
  QCheck.Test.make ~name:"seqadd_zeros" ~count:1000
    QCheck.(make
              ~print:show_sequence
              (Gen.pair integer_sequence_gen zeros_gen))
    (fun (seq,zeros) ->
      seqadd seq zeros = seq);;

(* adding 0 to each element in a sequence result in the same sequence,
   and hence the input sequence should be the same as the output sequence 
   e.g. seqadd [1;2;3] [0;0;0] => [1;2;3]
 *)
let seqadd_negate =
  let negate_list = List.map (fun x -> -x) in
  let sum = List.fold_left (+) 0
  in
  QCheck.Test.make ~name:"seqadd_negate" ~count:1000
    QCheck.(make integer_sequence_gen)
    (fun seq ->
      sum (seqadd seq (negate_list seq)) = 0);;

(* multiplying all elements by 0 should result in a sequence of 0s,
   and hence the sum of these values should also be 0 
   e.g. seqmult [1;2;3] [0;0;0] => [0;0;0]
        sum [0;0;0] => 0
*)
let seqmult_zeros =
  let sum = List.fold_left (+) 0
  in
  QCheck.Test.make ~name:"seqmult_zeros" ~count:1000
    QCheck.(make
              ~print:show_sequence
              (Gen.pair integer_sequence_gen zeros_gen))
    (* TODO *)
    (fun (seq,zeros) -> false);;

(* list of all property tests *)                  
let property_tests =
  List.map QCheck_ounit.to_ounit2_test
    [ seqadd_empty1
    ; seqadd_empty2
    ; seqadd_zeros
    ; seqmult_zeros
    ];;

(* run the unit and property based tests *)
let () =
  run_test_tt_main
    ("sequence_arithmetic_tests">:::
       (List.append unit_tests property_tests));;
