namespace Algorithms.DynamicProgramming

(* 
    Dynamic programming:  https://en.wikipedia.org/wiki/Dynamic_programming#Fibonacci_sequence
    Problem url: https://www.hackerrank.com/challenges/maxsubarray 
*)

module MaximumSubAray =
    open System 

    let maxSubArray l = 
        let rec loop isEmpty (sumCont: Numerics.BigInteger) (sumNoncont: Numerics.BigInteger) (sumAcc: Numerics.BigInteger) (l: int list) =
            match l with
                | x::xs ->
                    let x' = bigint x
                    let sumAcc = (sumAcc + x')
                    //Is contigous sum bigger than contigous sum: 
                    if  sumAcc > sumCont then
                        let sumNoncont = sumNoncont + (x')
                        loop false (sumAcc) sumNoncont sumAcc xs
                    //new item bigger than contigous sum:
                    elif x' > sumCont then
                        let sumNoncont = sumNoncont + (x')
                        loop false (x') sumNoncont (x') xs
                    else
                        loop isEmpty sumCont sumNoncont sumAcc xs
                | [] -> isEmpty , sumCont , sumNoncont
        
        let (isEmpty,  sumCont , sumNoncont) = loop true 0I 0I 0I l
        //If cont still empty (all items negative)
        if isEmpty then
            //pick the max
            let max = List.max l |> bigint 
            (max, max)
        else sumCont, sumNoncont
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
