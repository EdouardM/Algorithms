namespace Algorithms.DynamicProgramming

(* 
    Dynamic programming:  https://en.wikipedia.org/wiki/Dynamic_programming#Fibonacci_sequence
    Problem url: https://www.hackerrank.com/challenges/maxsubarray 
*)

module MaximumSubAray =
    open System 

    let maxSubArray l = 
        
        let rec loop (sumCont: Numerics.BigInteger) (sumNoncont: Numerics.BigInteger) (sumAcc: Numerics.BigInteger) (l: int list) =
            match l with
                | x::xs ->
                    let x' = bigint x
                    //If x' positive add to non contigous sum
                    let sumNoncont = if x' >= 0I then sumNoncont + (x') else sumNoncont

                    if sumAcc >= 0I then 
                        let sumAcc = (sumAcc + x')
                        //Is acc sum bigger than contigous sum: 
                        if  sumAcc > sumCont then
                            loop (sumAcc) sumNoncont sumAcc xs
                        else
                            loop sumCont sumNoncont sumAcc xs
                    //new item bigger than contigous sum:
                    elif x' > sumCont then
                        loop (x') sumNoncont (x') xs
                    else
                        loop sumCont sumNoncont sumAcc xs

                | [] -> sumCont , sumNoncont
        
        //If all items negative
        if List.forall(fun x -> x <= 0) l then
            //pick the max
            let max = List.max l |> bigint 
            (max, max)
        else 
            loop 0I 0I 0I l

    //Tests:
    maxSubArray [-1; 2; -3; 4; 5; -1; 7]



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
            printfn "%A %A" cont noncont
        ) |> ignore
        0
