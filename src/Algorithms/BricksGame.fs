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
    let bind f = function
            | Success x -> f x
            | Failure e -> Failure e

    let map f = function
            | Success x -> Success(f x)
            | Failure e -> Failure e

    let (>>=) = bind

    type Stack= StackContent of int list

    let empty = StackContent []

    let push x stack =
        let (StackContent l) = stack
        StackContent (x::l)

    let pop (StackContent l) =
        match l with
            | []    -> Failure "Stack underflow"
            | x::rest -> Success (x, StackContent rest)
    
    let remove1 = pop
    let remove2 stack = 
        pop
        >>= snd