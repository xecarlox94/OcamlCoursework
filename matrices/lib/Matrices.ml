
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
    (x::rest) -> x ;;  
    

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



(* test whether a list of lists of integers represents a matrix. 
   The length of each row should be equal.*)
let ismatrix x =
  match x with
    (IM []) -> true
    | (IM [[]]) -> true
    | x -> all_matrix_rows_same x ;;

(* function matrixshape takes the matrix, and calculates the number of
   columns and rows *)
let matrixshape x =
  match x with
    (IM []) -> (0,0)
    | (IM [[]]) -> (0, 0)
    | x -> ( (get_head (rows_length_list x)) , (count_rows_matrix x) ) ;;

(* matrix addition *)
let rec matrixadd x y =
  failwith "not implemented yet" ;;

(* matrix multiplication *)
let matrixmult x y =
  failwith "not implemented yet" ;;


             
