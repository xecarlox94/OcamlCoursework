
(* type declaration for intseq *)

type intseq = int list;;

let mInt = fun x y -> x * y ;;

(* implementations *)

(* function that adds two sequences from head to tail, summing at each
   position. *)
let rec seqadd : intseq -> intseq -> intseq =
  fun xs ys -> 
    match xs, ys with
      [x], [y] -> [( x + y )]
      | h1::rest1, h2::rest2 -> ( h1 + h2 ) :: seqadd rest1 rest2
      | xs, [] -> []
      | [], ys -> [] ;;

  
(* function that multiplies two lists from head to tail, multiplying
   at each position *)
let rec seqmult : intseq -> intseq -> intseq =
  fun xs ys -> match xs, ys with
    [x], [y] -> [( mInt x y )]
    | h1::rest1, h2::rest2 -> ( mInt h1 h2) :: seqmult rest1 rest2 ;;
