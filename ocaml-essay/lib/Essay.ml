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



(*
    The Ocaml language is a statically typed functional language that can infer the types of variables or functions on the go but it is a good practice to explicitly declare them. The Function type signatures define the function's arguments and return types. This is a good feature as it helps programmers to easily identify the behavior and the properties of a function just from the type signatures. There are two ways to declare type signatures.
    The first is to declare it between the function declaration (using a collumn) and body (using the equal sign), where arguments and the return types are separated by right arrows. The function arguments are later bound to the function inside the function’s body, using the fun keyword and a right arrow before the function's implementation. 
    
    The following function is an example of that, it is called mul_int and its arguments are two integers that will be multiplied it will return an integer. The arguments are bound inside the function's body using the fun keyword and the right arrow, before the implementation.
*)

let mul_int: int -> int -> int = fun x y -> x * y ;;

mul_int 7 9 ;;

(*
    The second way to do a function signature is to bind the arguments to the function variable and declare the arguments’ types inline inside its parenthesis separated by a column. The arguments remain separated by space character instead of the right arrows. The return type is then declared separated by a column, before the function’s body

    The next function uses this syntax to define a function that divides two floats and returns another float.
*)


let div_float (x: float) (y: float) : float = x /. y ;;

div_float 4.0 2.0 ;;

(*
    Although the function is statically typed it provides flexibility and reusability in the code as it supports polymorphism. This feature allows the programmers to generalise the arguments and return types. The advantage is to reuse the function for many different scenarios that match the same signature type pattern, without writing more code. The polymorphic type is defined using one single quotation mark before a generic character such as “a”, “b” or “c”. Any implementation of a polymorphic function must follow the generic types defined by using the same types either on the arguments or as the function result.
    The biggest advantage of this is when defining high order functions that apply the same functionality to different cases. The higher may or may not apply other functions specified in the arguments for certain scenarios.

    The high order count is an example of an high order function that count the number of elements on a list. This function is polymorphic because it can handle any kind of list, either integer or string list for example.
*)


let rec count: 'a list -> int = 
    fun xs -> 
        match xs with
            [] -> 0
            | (x::rest) -> 1 + (count rest) ;;

count [23;5;7;8] ;;

count ["dsf";"sdfs"; "sddfsf"] ;;