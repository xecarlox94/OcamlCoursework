
(* implement the function *)
let curry : (('a * 'b) -> 'c) -> 'a -> 'b -> 'c =
  fun f x y -> f (x , y) ;;

(* implement the function *)
let uncurry : ('a -> 'b -> 'c) -> ('a * 'b) -> 'c =
  fun f (x, y) -> f x y ;;
