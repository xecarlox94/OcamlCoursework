
(* i2c recursively applies a function f to a parameter x n times *)
let rec i2c i f x =
  match i with
    0 -> x
    | i -> i2c (i - 1) f (f x) ;;

(* passes the incrementing function and 0 into the Church numeral, so
   every application of f increases the output by 1. Since it starts
   at 0, this just returns the amount of times f was applied to x,
   thus telling us which Church numeral it was. *)
let c2i x =
  i2c x ( fun x -> x + 1 ) 0 ;;
