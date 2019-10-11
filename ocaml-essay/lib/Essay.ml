(* Write an essay on the ML type system.  Be clear, to-the-point, and concise.  Convince your 
 * marker that you understand:
 *                                                                                
 * - Function type signatures.  
 * - Polymorphism. 
 * - List types and tuple types (and their differences). 
 * - OCaml pattern-matching on values (e.g. integers) and structures (e.g. lists). 
 * - Named and anonymous functions. 
 * - Recursive functions. 
 * - Unit and property based tests. 
 * 
 * Include short code-fragments (as I do when lecturing) to illustrate
 * your observations.
 *
 * Do this in a literate programming style to include executable code
 * to demonstrate the concepts you're writing about, e.g.
 *)

(* The following function adds two numbers together. It has types ... *)

let add (x:int) (y:int) : int = x + y ;;

(* The following function... *)
