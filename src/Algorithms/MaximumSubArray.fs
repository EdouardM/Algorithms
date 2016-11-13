namespace Algorithms.DynamicProgramming

(* 
    Dynamic programming:  https://en.wikipedia.org/wiki/Dynamic_programming#Fibonacci_sequence
    Problem url: https://www.hackerrank.com/challenges/maxsubarray 
*)

module MaximumSubAray =
    open System 

    let maxSubArray l = 
        let rec loop (cont: int list) (noncont: int list) acc l =
            match l with
                | x::xs ->
                    let newAcc = x::acc
                    //Is contigous sum bigger than contigous sum: 
                    if List.sum newAcc > List.sum cont then
                        let noncont' = x::noncont
                        loop newAcc noncont' newAcc xs
                    //new item bigger than contigous sum:
                    elif x > List.sum cont then
                        let noncont' = x::noncont
                        loop [x] noncont' [x] xs
                    else
                        loop cont noncont newAcc xs
                | [] -> (cont, noncont)
        
        let cont, noncont = loop [] [] [] l
        //If cont still empty (all items negative)
        if List.isEmpty cont then
            //pick the max
            let max = List.max l 
            ([max], [max])
        else cont, noncont
        
    let solution l = 
        let cont, noncont = maxSubArray l
        List.sum cont, List.sum noncont
    let readList() = 
        let n = Console.ReadLine() |> int
        let arr = 
            Console.ReadLine().Split [|' '|] 
            |> Array.map int
            |> List.ofArray
        arr
        

    [<EntryPoint>]
    let main argv = 
        let t = Console.ReadLine() |> int
        List.init t (fun _ -> 
            let l = readList()
            let cont, noncont = solution l
            printfn "%d %d" cont noncont
        ) |> ignore
        0
