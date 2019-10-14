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
    The Ocaml language is a statically typed functional language that can infer the types of variables or functions on the go but it is a good practice to explicitly declare them in the constraint section. The Function type signatures constraints the function's arguments and return types. This is a good feature as it helps programmers to easily identify the behaviour and the properties of a function just from the type signatures. There are two ways to declare type signatures.
    The first is to declare the signature inside the function’s constraint. The function arguments are later bound to the function inside the function’s body.
    The second way to do a function signature is to bind the arguments to the function variable and declare the arguments’ types inline inside its brackets separated by a column. The arguments remain int the function’s constraint section. The return type is then declared separated by a column, before the function’s body.
    
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


(*
    In Ocaml, lists are polymorphic sequences and for that reason can hold any kind of type. Although lists are polymorphic, once the first element is inserted, all elements must be of the same type. Lists can be decomposed and constructor using the “cons” operator, to separate the head element of a list from the rest of the list or to join an element to a list and it is decomposed further until it reaches its initial nuclear value, an empty list “[ ]”. The application of const on lists is very important in OCaml programming as it allows to manipulate large volumes of data using specific functions, due to its iterative nature. The lists are declared using squared brackets and its values are separated by semicolumns.
    The tuple type also features in the language, and its purpose is to aggregate data values into one variable. The tuple is also a polymorphic data type and it even allows to aggregate different data types in the same variable, as long as the data types are uniform to its type signature declared upon its creation. The tuples can be constructed using the brackets and its values must be separated using commas. The tuples are not interactive as lists so they can not be used to store sequences.

    An example of two lists can be as it follows:
*)

let s_list = ( "dshsg" :: ( "asafasf" :: [] ) ) ;;

let s_float = ( 3.7 :: ( 2.6 ::  ( 3.1 :: [] ) ) ) ;;

(* Two example of tuples are the following *)

let tuple = (23, "dfaf", true) ;;

let tuple2 = ("sdfsa", "adsfdsf", "jtyjtyjtj") ;;



(*
    Match pattern is a feature that checks if the argument or multiple arguments against values, conditions or deconstructors. When the arguments are evaluated in a certain match pattern its expression runs, otherwise the arguments can be evaluated using the wildcard “_” that will run the default expression. Match Pattern is very useful for lists as it provides the functionality to perform list operations, along with the “cons” operator aid. This feature also allows tuples or any data type to be deconstructed, to extract data or other aggregated data types.

    The followig match pattern is testing the string value weather and if it does not match, the will card will "catch" any other value.
*)


let is_good_weather: string -> bool = 
    fun weather ->
        match weather with
            "sunny" -> true
            | "raining" -> false
            | _ -> false ;;

is_good_weather "sunny" ;;
is_good_weather "windy" ;;


(*
    These two function deconstructs a list. The first returns its head element and the second returns the rest of a list ( the whole list without the head element ). it uses the wild card combinated with the list to ignore the parts non-relevant for the purpose of this function
*)

let int_list = [7;1;2;3;4;5;9] ;;

let get_head: int list -> int = 
    fun list ->
    match list with
        (head::_) -> head
        | _ -> -1 ;;

get_head int_list;;


let get_rest: int list -> int list = 
    fun list ->
    match list with
        (_::rest) -> rest
        | _ -> [] ;;

get_rest int_list ;;

(*
    The following function deconstructs a tuple and returns its integer
*)

let tuple1: ( string * int) = ("Hi there", 123) ;;

let get_tuples_integer: (string * int) -> int =
    fun tuple ->
        match tuple with
            (str, int) -> int ;;

get_tuples_integer tuple1 ;;



