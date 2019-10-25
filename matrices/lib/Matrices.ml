
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
let get_head: 'a list -> 'a =
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

let rec count_rows_matrix: intseq list -> int =
  fun x ->
    match x with
      [] -> 0
      | [[]] -> 0
      | (row::rest) -> 1 + count_rows_matrix rest ;;

(* returns an list with the length of each row list *)
let rec rows_length_list: intseq list -> intseq =
  fun x ->
    match x with
      [] -> []
      | [[]] -> []
      | (row::rest) -> (count_row_elements row) :: rows_length_list rest ;;


(* checks if all list elements are equal to l and equal among eachother *)
let rec all_list_elem_same: int -> intseq -> bool =
  fun l xs ->
    match xs with
      [] -> true
      | [elem] -> (elem = l)
      | (x::rest) -> (x = l) && all_list_elem_same l rest ;;
    

(* checks if matrix rows have all the same length *)
let all_matrix_rows_same: intseq list -> bool =
  fun x ->
    all_list_elem_same (get_head (rows_length_list x)) (rows_length_list x) ;;


(* test whether a list of lists of integers represents a matrix. 
   The length of each row should be equal.*)
let ismatrix x =
  match x with
    (IM []) -> true
    | (IM [[]]) -> true
    | (IM intseqs) -> all_matrix_rows_same intseqs ;;

(* function matrixshape takes the matrix, and calculates the number of rows and collumns *)
let matrixshape x =
  match x with
    (IM []) -> (0,0)
    | (IM [[]]) -> (1, 0)
    | ( IM intseqs )-> ( (count_rows_matrix intseqs), (get_head (rows_length_list intseqs)) ) ;;

(* adding two intseqs, integer by integer, and returns a intseq sum of each element *)
let rec add_two_intseqs: intseq -> intseq -> intseq =
  fun intseq1 intseq2 ->
    match intseq1, intseq2 with
      [], [] -> []
      | (head1::rest1), (head2::rest2) -> ( head1 + head2 ) :: ( add_two_intseqs rest1 rest2 ) ;; 

(* adding two intseq lists, intseq by intseq, and returns a inteseq list with a sum of each intseqs *)
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
  | IM integerseqlist1, IM integerseqlist2 -> IM ( add_two_intseq_lists integerseqlist1 integerseqlist2 ) ;;

(* takes an integer from an intseq *)
let rec take_elem_pos: int -> intseq -> int =
  fun elem intseq1 ->
    match intseq1 with
      (x::rest) ->
        if ( elem = 0)
        then x
        else take_elem_pos ( elem - 1) rest
      | _ -> -1 ;;

(* builds an intseq from many different lists, from a specific index *)
let rec build_new_row: int -> intseq list -> intseq =
  fun elem intseqs ->
    match intseqs with
      [] -> []
      | (row::rest) -> ( take_elem_pos elem row ) :: build_new_row elem rest ;;

(* gets the length of a intseq  *)
let intseqlist_collumnlength: intseq list -> int =
  fun intseq_list ->
    let shape: (int * int) = matrixshape (IM intseq_list) 
    in
    match shape with
      ( rowlength, collumnlength ) -> collumnlength ;;

(* transpose a intseq list with a given collumn length *)
let transpose: int -> int -> intseq list -> intseq list =
  fun length_collumns counter intseqs ->
    let rec transpose_rec: int -> int -> intseq list -> intseq list =
      fun length_collumns counter intseqlist ->
        if ( (length_collumns - 1 )  > counter)
        then (build_new_row counter intseqlist) :: transpose_rec length_collumns  (counter + 1) intseqlist
        else [build_new_row counter intseqlist]
    in
    transpose_rec length_collumns 0 intseqs ;;

(* transpose a intseq list *)
let transpose_intseq_list: intseq list -> intseq list =
  fun intseqs ->
    let length_collumns = intseqlist_collumnlength intseqs 
    in
    transpose length_collumns 0 intseqs ;;  

(* it multiplies two inteseqs and returns an integer *)
let rec mul_intseqs: intseq -> intseq -> int =
  fun intseq1 intseq2 ->
    match intseq1, intseq2 with
      [x1], [x2] -> ( x1 * x2 )
      | (h1::rest1), (h2::rest2) -> ( h1 * h2 ) + (mul_intseqs rest1 rest2)
      | _, _ -> 0;;


(* it multiplies an intseq by a intseq list and returns a intseq *)
let rec mult_intseq_by_intseqlist: intseq -> intseq list -> intseq =
  fun intseq intseqlist ->
    match intseqlist with
      [row] -> [mul_intseqs intseq row]
      | (headrow::rest) -> (mul_intseqs intseq headrow) :: (mult_intseq_by_intseqlist intseq rest) ;;

(* multiply two intseq lists and returns a single intseq list *)
let rec mul_intseqlists: intseq list -> intseq list -> intseq list =
  fun intseq1 intseq2 ->
    match intseq1 with
      [row] -> [mult_intseq_by_intseqlist row intseq2]
      | (headrow::rest) -> ( mult_intseq_by_intseqlist headrow intseq2) :: (mul_intseqlists rest intseq2) ;;


(* matrix multiplication *)
let matrixmult x y =
  match x, y with
    IM [], IM [] -> IM []
    | IM [[]], IM [[]] -> IM [[]]
    | IM integerseq1, IM integerseq2 -> IM ( mul_intseqlists integerseq1 integerseq2 ) ;;


