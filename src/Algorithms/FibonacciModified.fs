namespace Algorithms.DynamicProgramming

(*
    Problem url: https://www.hackerrank.com/challenges/fibonacci-modified
     
*)

module FiboModified =
    open System 

    let fibo (t1: Numerics.BigInteger) (t2: Numerics.BigInteger) n = 
        Seq.unfold(fun (i, t1, t2)  -> 
            if i <= n then 
                let res = t1 + pown t2 2
                Some ( res, (i + 1, t2, res))
            else None) (0, t1, t2)
    
    let solution ( (t1, t2, n) : (int * int * int) )  = 
        let t1' = bigint t1
        let t2' = bigint t2
        fibo t1' t2' n
        |> Seq.item (n - 3)

    let readInput() = 
        let [|t1; t2; n|] = Console.ReadLine().Split[|' '|] |> Array.map int
        t1, t2, n

    [<EntryPoint>]
    let main argv = 
        readInput()
        |> solution
        |> printfn "%A"
        0 
