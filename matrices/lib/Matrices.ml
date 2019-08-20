
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

(* test whether a list of lists of integers represents a matrix. 
   The length of each row should be equal.*)
let ismatrix x =
  failwith "not implemented yet" ;;

(* function matrixshape takes the matrix, and calculates the number of
   columns and rows *)
let matrixshape x =
  failwith "not implemented yet" ;;

(* matrix addition *)
let rec matrixadd x y =
  failwith "not implemented yet" ;;

(* matrix multiplication *)
let matrixmult x y =
  failwith "not implemented yet" ;;


             
