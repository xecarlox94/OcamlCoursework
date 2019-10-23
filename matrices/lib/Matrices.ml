
(* declaration of types intseq and intmatrix *)

type intseq = int list;;

type intmatrix = IM of intseq list;;

(* useful for debugging *)
let string_of_row row =
  String.concat ""
    (List.map (fun x -> string_of_int x ^ " ") row);;

(* useful for debugging *)
let rec string_of_matrix m =
  match m with
          [] -> ""
      | [[]] -> ""
      | (row::rest) ->
         string_of_row row ^ "\n" 
         ^ string_of_matrix rest;;

(* function getbody to retrieve the body of the intmatrix which is of type intseq list *)
let getbody (IM x) = x;;

(* Gets head of a integer list *)
let get_head: int list -> int =
  fun xlist ->
    match xlist with
      (x::rest) -> x
      | _ -> -1 ;;  
    

(* counts elements in a list *)
let rec count_row_elements: 'a list -> int =
  fun row ->
    match row with
      [] -> 0
      | (x::rest) -> 1 + count_row_elements rest ;;

let rec count_rows_matrix: intmatrix -> int =
  fun x ->
    match x with
      IM [] -> 0
      | IM [[]] -> 0
      | IM (row::rest) -> 1 + count_rows_matrix ( IM rest ) ;;

(* returns an list with the length of each row list *)
let rec rows_length_list: intmatrix ->int list =
  fun x ->
    match x with
      IM [] -> []
      | IM [[]] -> []
      | IM (row::rest) -> (count_row_elements row) :: rows_length_list (IM rest) ;;


(* checks if all list elements are equal to l and equal among eachother *)
let rec all_list_elem_same: int -> int list -> bool =
  fun l xs ->
    match xs with
      [] -> true
      | [elem] -> (elem = l)
      | (x::rest) -> (x = l) && all_list_elem_same l rest ;;
    

(* checks if matrix rows have all the same length *)
let all_matrix_rows_same: intmatrix -> bool =
  fun x ->
    all_list_elem_same (get_head (rows_length_list x)) (rows_length_list x) ;;


(* test whether a list of lists of integers represents a matrix. 
   The length of each row should be equal.*)
let ismatrix x =
  match x with
    (IM []) -> true
    | (IM [[]]) -> true
    | x -> all_matrix_rows_same x ;;

(* function matrixshape takes the matrix, and calculates the number of columns and rows *)
let matrixshape x =
  match x with
    (IM []) -> (0,0)
    | (IM [[]]) -> (0, 0)
    | x -> ( (get_head (rows_length_list x)) , (count_rows_matrix x) ) ;;


let rec add_two_intseqs: intseq -> intseq -> intseq =
  fun intseq1 intseq2 ->
    match intseq1, intseq2 with
      [], [] -> []
      | (head1::rest1), (head2::rest2) -> ( head1 + head2 ) :: ( add_two_intseqs rest1 rest2 ) ;; 

let rec add_two_intseq_lists: intseq list -> intseq list -> intseq list =
  fun int_seq_list1 int_seq_list2 -> 
    match int_seq_list1, int_seq_list2 with
      [], [] -> []
      | [[]], [[]] -> [[]]
      | (row1::rest1), (row2::rest2) -> ( add_two_intseqs row1 row2 ) :: ( add_two_intseq_lists rest1 rest2 ) ;;


(* matrix addition *)
let matrixadd x y =
  match x, y with
  IM [], IM [] -> IM []
  | IM [[]], IM [[]] -> IM [[]]
  | IM integerseq1, IM integerseq2 -> IM ( add_two_intseq_lists integerseq1 integerseq2 ) ;;


(* matrix multiplication *)
let matrixmult x y = 
  failwith "not implemented yet" ;;


