module type STACK = sig
  (** ['a t] is the type of stacks with element type ['a]. *)
  type 'a t
  
  (** [empty] is an empty stack. *)
  val empty : 'a t
  
  (** [is_empty] checks if a stack is empty. *)
  val is_empty : 'a t -> bool
  
  (** [push x s] places an element [x] on top of stack [s] and returns the new stack. *)
  val push : 'a -> 'a t -> 'a t
  
  (** [peek s] looks at the top element of stack [s]. If the stack is empty, [None] is returned. *)
  val peek : 'a t -> 'a option
  
  (** [pop s] removes the top element of stack [s] and returns the resulting stack. If the stack is already empty, [None] is returned. *)
  val pop : 'a t -> 'a t option
  
  (** [size s] returns the number of elements in the stack. *)
  val size : 'a t -> int
end

(** [size s] returns the number of elements in the stack. *)
let size s = List.length s

(** The type of recipes for building a stack. *)
type 'a build =
  | Empty
  (** [Empty] means calling [empty] to get a stack. *)
  | Push of 'a * 'a build
  (** [Push (x, b)] means building a stack [s] using [b] and then calling [push x s] to get a stack. *)
  | Pop of 'a build
  (** [Pop b] means building a stack [s] using [b] and then calling [pop s] to get a stack. If [pop s] returns [None], the construction fails and an exception must be raised. *)

(** The type of checks on a stack. *)
type 'a check =
  | IsEmpty of bool
  (** [IsEmpty b] means that calling [is_empty] on the stack should get the response [b]. *)
  | Peek of ('a -> bool) option
  (** [Peek None] means that calling [peek] on the stack should get [None]. [Peek (Some p)] means that calling [peek] on the stack should get [Some x] and [p x] should be true. *)
  | Pop of unit option
  (** [Pop None] means that calling [pop] on the stack should get [None]. [Pop (Some ())] means that calling [pop] on the stack should get [Some s] for some stack [s]. *)
  | Size of int
  (** [Size i] means that calling [size] on the stack should get the response [i]. *)

(** A module for verifying the correctness of a [STACK] implementation. *)
module StackTester (Stack : STACK) = struct
  exception BuildFailure

  (** Creation of the [build] function to allow stack construction using a recipe as in the given example *)
  let build recipe =
    let rec build' stack = function
      | Empty -> stack
      | Push (x, r) -> build' (Stack.push x stack) r
      | Pop r -> 
          match Stack.pop stack with
          | None -> raise BuildFailure
          | Some s -> build' s r
    in build' Stack.empty recipe

  (** Creation of the [test] function to allow checking if the stack meets certain conditions *)
  let test stack = function
    | IsEmpty b -> Stack.is_empty stack = b
    | Peek None -> Stack.peek stack = None
    | Peek (Some p) ->
        (match Stack.peek stack with
         | None -> false
         | Some x -> p x)
    | Pop None -> Stack.pop stack = None
    | Pop (Some ()) -> (match Stack.pop stack with | None -> false | Some _ -> true)
    | Size i -> Stack.size stack = i

  (** Creation of the [print_stack] function to allow printing of stack [s]. Uses [List.rev] to read the stack from left to right *)
  let print_stack s =
    if List.length s = 0 then
      print_endline "empty stack"
    else
      let stack_str = List.fold_left (fun acc x -> acc ^ string_of_int x ^ ", ") "[" (List.rev s) in
      let stack_str = String.sub stack_str 0 (String.length stack_str - 2) in
      print_endline (stack_str ^ "]")
end

(** Creation of a structure to allow stack construction without a recipe *)
module MyStack : STACK with type 'a t = 'a list = struct
  type 'a t = 'a list

  let empty = []
  let is_empty s = s = []
  let push x s = x :: s
  let peek = function
    | [] -> None
    | x :: _ -> Some x
  let pop = function
    | [] -> None
    | _ :: xs -> Some xs 
  let size s = List.length s
end

(** funcao que nos permite ver se determinado valor é um digito*)
let is_digit = function '0' .. '9' -> true | _ -> false

module MyStackTester = StackTester(MyStack)

(** Creation of the stacks used in our program *)

let stack = MyStack.empty
let test1 = Push (5, Push (6, Push (8, (Pop (Pop (Empty))))))
let test2 = Push (2, Push (1, Push (5, Push (9, Push (10, Pop (Pop (Push (8, (Pop (Pop (Push (3, (Empty)))))))))))))
let test3 = Push (8, Push (99, Push (22, Pop (Pop (Pop (Push (6, Push (4, (Push (2, (Pop (Pop (Pop (Empty))))))))))))))
let test4 = Push (99, Pop (Push (77, (Pop (Push (55, (Pop (Push (66, (Push (67, Empty)))))))))))
let test5 = Push (65, Push (66, Push (67, Push (68, Push (69, Push (70, Push (72, Push (72, Push (73, Push (74, Push (75, Empty)))))))))))
let ex_test1 = MyStackTester.build test1
let ex_test2 = MyStackTester.build test2
let ex_test3 = MyStackTester.build test3
let ex_test4 = MyStackTester.build test4
let ex_test5 = MyStackTester.build test5

(** Menu creation *)

let rec menu stack =
  Printf.printf "\n******************************************\n";
  Printf.printf "\n******* Delivery Management - Truck 1 ******\n";
  Printf.printf "\n******************************************\n";
  Printf.printf "\n1 - Initialize empty stack                \n";
  Printf.printf "2 - Test if stack is empty                \n";
  Printf.printf "3 - Insert a value at the top of the stack\n";
  Printf.printf "4 - Get stack size                        \n";
  Printf.printf "5 - Get value at the top of the stack     \n";
  Printf.printf "6 - Print stack                           \n";
  Printf.printf "7 - Run tests                             \n";
  Printf.printf "8 - Pop the stack                         \n";
  Printf.printf "\n******************************************\n";
  try
    let option = read_int () in
    match option with
    | 1 ->
        let stack = MyStack.empty in
        ignore @@ Sys.command "cls"; (** function that clears the screen *)
        Printf.printf "\nStack successfully initialized.\n";
        menu stack

    | 2 ->
        if MyStackTester.test stack (IsEmpty true) then
          (ignore @@ Sys.command "cls";
           Printf.printf "\nThe stack is empty.\n")
        else
          (ignore @@ Sys.command "cls";
           Printf.printf "\nThe stack is not empty.\n");
        menu stack

    | 3 ->
        Printf.printf "\nEnter a value to insert into the stack (enter an integer value)\n";
        let input = read_line () in
        if String.for_all (fun c -> is_digit c || c = '-') input then (* checks if all characters are digits or the minus sign *)
          let value = int_of_string input in
          let stack = MyStack.push value stack in (* adds the value to the stack *)
          ignore @@ Sys.command "cls";
          Printf.printf "\nThe value %d was inserted into the stack\n" value;
          menu stack
        else
          (ignore @@ Sys.command "cls";
           Printf.printf "\nInvalid input. Please enter an integer value\n";
           menu stack)

    | 4 ->
        if MyStackTester.test stack (IsEmpty true) then
          (ignore @@ Sys.command "cls";
           Printf.printf "\nThe stack is empty.\n")
        else
          let size = MyStack.size stack in
          ignore @@ Sys.command "cls";
          Printf.printf "\nThe stack size is %d\n" size;
        menu stack

    | 5 ->
        (match MyStack.peek stack with
         | None -> ignore @@ Sys.command "cls"; Printf.printf "\nThe stack is empty.\n"
         | Some x -> ignore @@ Sys.command "cls"; Printf.printf "\nThe top element of the stack is %d\n" x);
        menu stack

    | 6 ->
        ignore @@ Sys.command "cls";
        MyStackTester.print_stack stack;
        print_newline ();
        menu stack

    | 7 ->
        ignore @@ Sys.command "cls";
        print_newline ();
        Printf.printf "Test 1:\n";
        MyStackTester.print_stack ex_test1;
        print_newline ();
        Printf.printf "Test 2:\n";
        MyStackTester.print_stack ex_test2;
        print_newline ();
        Printf.printf "Test 3:\n";
        MyStackTester.print_stack ex_test3;
        print_newline ();
        Printf.printf "Test 4:\n";
        MyStackTester.print_stack ex_test4;
        print_newline ();
        Printf.printf "Test 5:\n";
        MyStackTester.print_stack ex_test5;
        menu stack

    | n when n < 1 ->
        ignore @@ Sys.command "cls"; Printf.printf "\nInvalid option. Please enter a number between 1 and 8.\n"; menu stack
    | n when n > 8 ->
        ignore @@ Sys.command "cls"; Printf.printf "\nInvalid option. Please enter a number between 1 and 8.\n"; menu stack

        | 8 -> (match MyStack.pop stack with
        | None -> ignore @@ Sys.command "cls"; Printf.printf "\nCannot pop because the stack is empty.\n"; menu stack
        | Some x -> ignore @@ Sys.command "cls"; let stack = x in
                    Printf.printf "\nThe top element of the stack has been removed.\n";
                    menu stack)
    | _ -> ignore @@ Sys.command "cls"; Printf.printf "\nInvalid option. Please enter a number between 1 and 8.\n"; menu stack
    with
    | Failure msg when msg = "int_of_string" ->
        ignore @@ Sys.command "cls"; Printf.printf "\nInvalid input. Please enter a number between 1 and 8.\n"; menu stack
    | ex -> raise ex;;
    
    menu stack;;
    