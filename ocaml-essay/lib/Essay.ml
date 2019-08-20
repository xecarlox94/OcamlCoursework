(* Write an essay on the ML type system.  Be clear, to-the-point, and concise.  Convince your 
 * marker that you understand:
 *                                                                                
 * - Ad-hoc and parametric polymorphism.
 * - Function types.
 * - List types and tuple types (and their differences).
 * - Equality types.
 * - ML patterns and pattern-matching.
 * - Unit testing (test/EssayTests.ml).
 * - Property based testing (test/EssayTests.ml).
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
