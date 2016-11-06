namespace Algorithms.DynamicProgramming

(*
    Problem url: https://www.hackerrank.com/challenges/unbounded-knapsack
    
    Our problem is an unbounded knapsack problem:    
    Solution Wikipedia:  https://en.wikipedia.org/wiki/Knapsack_problem 
*)

module Knapsack =
    open System 

    // Here weight and value are the same and are all >= 0
    // We can repeat items as many times as necessary - unbounded
    let rec loop weights maxWeight i acc =
        match i with
            | 0 -> 
                    //Sum of 0 item  = 0
                    let newAcc = Map.add 0 0 acc
                    loop weights maxWeight (i + 1) newAcc 
            //Return last solution i > maxWeight
            | i when i > maxWeight -> acc.[maxWeight]
            | i ->
                    let weights' = List.filter(fun w -> w <= i) weights
                    let newAcc = 
                        if weights'.Length > 0 then
                            let res =
                                weights'    
                                |> List.map(fun w -> w + acc.[i - w])
                                |> List.max
                            printfn "i: %d res: %d" i res
                            Map.add i res acc
                        else 
                            //did not find correct weight put 0 in acc
                            Map.add i 0 acc
                    loop weights maxWeight (i + 1) newAcc

    let knapSack (maxWeight, weights) = loop weights maxWeight 0 Map.empty 

    let readArray() = Console.ReadLine().Split [| ' '|] |> Array.map int
    let readTestCase() = 
        let [|n; maxWeight|] = readArray()
        let weights = readArray()
        maxWeight, weights

    let readInput() = 
        let t = Console.ReadLine() |> int
        Array.init t (fun _ -> readTestCase() )
    
    let solution= 
        readInput
        >> Array.map (fun (maxWeight , weights) -> maxWeight, weights |> List.ofArray)
        >> Array.map  knapSack
        >> Array.map (printfn "%d")
        >> ignore

    [<EntryPoint>]
    let main argv =
        solution() 
        0

                    
        




    
    
