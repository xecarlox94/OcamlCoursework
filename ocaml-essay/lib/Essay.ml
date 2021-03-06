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
    ** Function type signatures **

    The Ocaml language is a statically typed functional language that can infer the types of variables or functions on the go but it is a good practice to explicitly declare them in the constraint section. The Function type signatures constraints the function's arguments and return types. This is a good feature as it helps programmers to easily identify the behaviour and the properties of a function just from the type signatures. There are two ways to declare type signatures.
    The first is to declare the signature inside the function’s constraint. The function arguments are later bound to the function inside the function’s body.
    The second way to do a function signature is to bind the arguments to the function variable and declare the arguments’ types inline inside its brackets separated by a column. The arguments remain int the function’s constraint section. The return type is then declared separated by a column, but still inside the function's constraints.
    
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
    ** Polymorphism **

    Although the function is statically typed it provides flexibility and reusability in the code as it supports polymorphism. This feature allows the programmers to generalise the arguments and return types. The advantage is to reuse the function for many different scenarios that match the same signature type pattern, without writing more code. The polymorphic type is defined using one single quotation mark before a generic character such as “a”, “b” or “c”. Any implementation of a polymorphic function must follow the generic types defined by using the same types either on the arguments or as the function return result.
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
    ** List types and tuple types **

    In Ocaml, lists are polymorphic sequences and for that reason can hold any kind of type. Although lists are polymorphic, once the first element is inserted, all elements must be of the same type. Lists can be decomposed and constructor using the “cons” operator, to separate the head element of a list from the rest of the list or to join an element to a list and it is decomposed further until it reaches its initial nuclear value, an empty list “[ ]”. The application of const on lists is very important in OCaml programming as it allows to manipulate large volumes of data using specific functions, due to its iterative nature. The lists are declared using squared brackets and its values are separated by semicolumns.
    The tuple type also features in the language, and its purpose is to aggregate data values into one variable. The tuple is also a polymorphic data type and it even allows to aggregate different data types in the same variable, as long as the data types are uniform to its type signature declared upon its creation. The tuples can be constructed using the brackets and its values must be separated using commas. The tuples are not interative as lists so they can not be used to store sequences.

    An example of two lists can be as it follows:
*)

let s_list = ( "dshsg" :: ( "asafasf" :: [] ) ) ;;

let s_float = ( 3.7 :: ( 2.6 ::  ( 3.1 :: [] ) ) ) ;;

(* Two example of tuples are the following *)

let tuple = (23, "dfaf", true) ;;

let tuple2 = ("sdfsa", "adsfdsf", "jtyjtyjtj") ;;



(*
    ** OCaml pattern-matching on values and structures **

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


(*
    ** Named and anonymous functions **

    Named and Anonymous functions have different purposes. Essentially when a function is named it is stored in a variable and it can be called and applied throughout the program. Another technique which is part of the named function is the partial application, that consists of storing an existing function, with more than one argument, and creating a new function composed of the old function applying a specific argument.
    Otherwise, the function might not need to be named or stored, mostly when it is used in one specific case or if it used inside another regular function or even a high-order function. An anonymous function is defined as a lambda expression, using the “fun” keyword to bind the variables from outside the function.

    The next functions is a normal named functions. The second one is also a named function but resultant from a partially applied named function.
*)


(* using the previous mul_int function *)

mul_int 5 6 ;;

let mul_int_by_100 = mul_int 100 ;;

mul_int_by_100 56;;

(*
    The following function is a map higher function which will apply a anonymous function to every element of the array.
*)

let rec map:  'a list -> ( 'a -> 'b ) -> 'b list = 
    fun list f ->
    match list with
        [] -> []
        | (head::rest) -> (f head) :: (map rest f) ;;

map int_list (fun x -> x * 5) ;;


(*
    ** Recursive functions **

    Recursiveness is essential for functional programming since it is not possible to perform loops or most control flow tecniques, since functional programming is part of the declarative paradigm of programming. The declarative paradigm expresses the computations needed to run but it does not describe the control flow of the program. 
    Recursion is then an crucial tecnique, for functional and declarative programming languages, as it performs repetitive tasks specified by computational expressions until a certain base case stops the repetition its own repetition, avoiding a potential infinite loop. In Recursion, every case possible must be expressed, in order to avoid unexpected infinite computation, and the base case should be the first in the case order. The reason behind the recursion order so the function checks if it is final result has been reached before any other case avoiding, as well, infinite computation.
    In Ocaml programming language, recursion is mostly applied in lists and it is the fundamental tecnique to manipulate them. It is possible to express a simple "if else" control flow statements inside the function's body, although it is good pratice, in functional programming, to use pattern matching since it checks exaustively for any possible pattern case on values or structures. An Ocaml recursive function needs to be declared with an additional "rec" keyword to infer the compiler or interpreter that this function will call itself muliple times, in its body.


    The following function, the filter funtion, will recursively decompose the elements of a list and return a list that with element that match a boolean function's condition.

*)


let rec filter: 'a list -> ( 'a -> bool ) -> 'a list =
     fun list f ->
        match list with
            [] -> []
            | (head::restx) when (f head) = true -> head :: (filter restx f)
            | (head::restx) -> filter restx f ;;


(* this filter function will return a integer sequence list, containing even integers only, as a result of using a anonymous function that asserts each integer *)

let only_evens = filter int_list (fun x -> (x mod 2) = 0) ;;


(*
    ** Unit and property based tests **

    Ocaml has support for unit based testing. This kind of testing test each function with a specific case. Each case is specific with unique values and its range is very limited, since it is very time consuming or imposible to test all possible cases, manually unit testing every single case.
    As a result of Ocaml being a functional programming, it has support for property based testing. Property testing has an incomparable more extensive and wider range than unit testing since the values being tested are randomly assigned by a library as well as the amount of test cases. Additionally to the variable automation, the property based testing library uses bound values to reduce the time to report major programming bugs or logical innacuracies. 
    Property based testing is only possible in functionally programming, due to the strict use of immutable variables and lambda computation as well as the absence of blobal state variables or structures. Immutable variables assure the programmer that the the memory address values will not change in any circunstance and, as a result, the result of any function is not affected by any paralel computation since there is none. The functional programming languages being lambda computation based, ensures that the order of processing does not matter as, according with the church-russel theorem, it will always return the exact same result, in any order of computation. Additionally lambda calculus also determines that functional programming is composed solely by functions, and any functions has the only purpose of taking input and returning input. For this last reason, there is no global state variables or structures being manipulated hence the predictness and absence of second effects on resuts necessary for property based testing.

*)