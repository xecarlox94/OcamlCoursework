
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
let get_head (xlist: int list): int =
  match xlist with
    (x::rest) -> x
    | _ -> -1 ;;  
    

(* counts elements in a list *)
let rec count_row_elements (row: 'a list): int =
  match row with
    [] -> 0
    | (x::rest) -> 1 + count_row_elements rest ;;

let rec count_rows_matrix (x: intmatrix): int =
      match x with
        IM [] -> 0
        | IM [[]] -> 0
        | IM (row::rest) -> 1 + count_rows_matrix ( IM rest ) ;;

(* returns an list with the length of each row list *)
let rec rows_length_list (x: intmatrix): int list =
  match x with
    IM [] -> []
    | IM [[]] -> []
    | IM (row::rest) -> (count_row_elements row) :: rows_length_list (IM rest) ;;


(* checks if all list elements are equal to l and equal among eachother *)
let rec all_list_elem_same l (xs: int list) : bool =
  match xs with
    [] -> true
    | [elem] -> (elem = l)
    | (x::rest) -> (x = l) && all_list_elem_same l rest ;;
    

(* checks if matrix rows have all the same length *)
let all_matrix_rows_same (x: intmatrix) =
  all_list_elem_same (get_head (rows_length_list x)) (rows_length_list x) ;;

(* sums two intseq, element by element, and returns a intseq  *)
let rec sum_two_intseq: intseq -> intseq -> intseq =
  fun xs ys ->
    match xs, ys with
      [x], [y] -> [( x + y )]
      | (h1::rest1), (h2::rest2) -> (h1 + h2) :: sum_two_intseq rest1 rest2
      | _ -> [] ;;


(* takes an integer from an intseq
let rec take_elem_pos (elem: int ) (list: intseq) =
  match list with 
    (x::rest) -> 
      if (elem = 0)
      then x
      else take_elem_pos (elem - 1) rest
    | _ -> -1 ;; *)

(* builds an intseq from many different lists, from a specific index
let rec build_new_row (elem: int) (list: intseq list) : intseq =
  match list with
    [] -> []
    | (head::rest) -> (take_elem_pos elem head) :: build_new_row elem rest;; *)


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

(* matrix addition *)
let rec matrixadd x y =
  match x, y with
    (IM [row1]), (IM [row2]) -> (sum_two_intseq row1 row2)
    | (rowx::restx), (rowy::resty) ->
      (sum_two_intseq rowx rowy) :: ( getbody (matrixadd ( IM restx) (IM resty)) ) ;;


(* matrix multiplication *)
let matrixmult x y = 
  failwith "not implemented yet" ;;


