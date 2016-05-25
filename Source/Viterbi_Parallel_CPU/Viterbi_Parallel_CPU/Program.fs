module Viterbi_Parallel_CPU

type msg =
    | Data of int * int
    | End

let is = ref false
let maxFun (arr : array<double>) =
    let mutable mx = 0.0
    let mutable num = 0
    for i in 0..arr.Length - 1 do
        if mx < arr.[i]
        then 
            mx <- arr.[i]
            num <- i
        else
            do()
    (mx, num)



let viterbi (observSpace: int[]) stateCount (startProbs : double[])  (observSeq : int[]) (transitionProbs : double[][]) (emissionProbs : double[][]) =
    let hiddenStateSeq = [|0..observSeq.Length - 1|]
    let z = [|0..observSeq.Length - 1|]

    let tableMax = [|for i in 0..stateCount - 1 -> 
                       [|for j in 0..observSeq.Length - 1 ->
                           if j = 0
                           then startProbs.[i] * emissionProbs.[i].[observSeq.[0]]
                           else 0.0|] |]
    let tableArgMax = Array.init stateCount (fun _ -> Array.zeroCreate observSeq.Length)

    let agent num = 
        MailboxProcessor.Start(fun inbox ->
         let rec loop n =
               async {
                       let! msg = inbox.Receive()
                       match msg with
                       |Data (i, j) ->
                            match maxFun [|for k in 0..stateCount - 1 -> tableMax.[k].[i - 1] * transitionProbs.[k].[j] * emissionProbs.[j].[observSeq.[i]]|] with
                            |(maxVal, maxNum) ->
                                tableMax.[j].[i] <- maxVal
                                tableArgMax.[j].[i] <- maxNum
                       |End -> is := true 
                       }
         loop 0)
    
    let streams = [|for i in 0..stateCount - 1 -> agent i|]

    for i in 1..observSeq.Length - 1 do
        for j in 0..stateCount - 1 do
            (streams.[j]).Post (Data (i, j))
        while not !is do()
    for i in 0..stateCount - 1 do
        (streams.[i]).Post End
    z.[observSeq.Length - 1] <- Array.maxBy (fun k -> tableMax.[k].[observSeq.Length - 1]) [|0..stateCount - 1|]
    hiddenStateSeq.[observSeq.Length - 1] <- z.[observSeq.Length - 1]
    for i in 1..(observSeq.Length - 1) do
        z.[observSeq.Length - i - 1] <- tableArgMax.[z.[observSeq.Length - i]].[observSeq.Length - i]
        hiddenStateSeq.[observSeq.Length - i - 1] <- z.[observSeq.Length - i - 1]
    hiddenStateSeq

    
