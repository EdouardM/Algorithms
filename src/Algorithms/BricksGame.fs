namespace Algorithms.DynamicProgramming

(*
    Problem url: https://www.hackerrank.com/challenges/play-game
     
*)

module BricksGame =
    open System 

    type FailureMessage = string
    type Result<'T> = 
        | Success of 'T
        | Failure of FailureMessage

    type Stack= StackContent of int list

    let empty = StackContent []

    let push x stack =
        let (StackContent l) = stack
        StackContent (x::l)

    let pop (StackContent l) =
        match l with
            | []    -> Failure "Stack underflow"
            | x::xs -> Success (x, StackContent xs)
    
    let remove1 = pop
    let remove2 stack = 
        pop stack
        
