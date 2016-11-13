namespace Algorithms.DynamicProgramming

(* 
    Dynamic programming:  https://en.wikipedia.org/wiki/Dynamic_programming#Fibonacci_sequence
    Problem url: https://www.hackerrank.com/challenges/maxsubarray 
*)

module MaximumSubAray =
    open System 

    let maxSubArray l = 
        let rec loop (cont, sumCont) (noncont: int list) (acc, sumAcc) l =
            match l with
                | x::xs ->
                    let newAcc = x::acc
                    let sumAcc = (sumAcc + x)
                    //Is contigous sum bigger than contigous sum: 
                    if  sumAcc > sumCont then
                        let noncont' = x::noncont
                        loop (newAcc, sumAcc) noncont' (newAcc, sumAcc) xs
                    //new item bigger than contigous sum:
                    elif x > sumCont then
                        let noncont' = x::noncont
                        loop ([x], x) noncont' ([x], x) xs
                    else
                        loop (cont, sumCont) noncont (newAcc, sumAcc) xs
                | [] -> ( (cont, sumCont) , noncont)
        
        let ((cont, sumCont) , noncont) = loop ([], 0) [] ([], 0) l
        //If cont still empty (all items negative)
        if List.isEmpty cont then
            //pick the max
            let max = List.max l 
            (max, max)
        else sumCont, List.sum noncont
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
            let cont, noncont = maxSubArray l
            printfn "%d %d" cont noncont
        ) |> ignore
        0
