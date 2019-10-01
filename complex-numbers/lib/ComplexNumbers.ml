type complex_number = CI of int*int;;

(* auxiliar functions *)
(* multiplies two integers *)
let mInt = fun x y -> x * y ;;

(* implementations *)

(* addition of two Complex Integers *)
let cadd (CI(x,y)) (CI(x2,y2)) = CI( ( x + x2), ( y + y2)) ;;

(* multiplication of two Complex Integers *)
let cmult (CI(x,y)) (CI(x2,y2)) =
  CI( ( (mInt x x2) - (mInt y y2) ), ( (mInt x y2) + (mInt y x2)) ) ;;

