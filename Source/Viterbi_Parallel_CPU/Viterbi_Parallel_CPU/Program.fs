module Viterbi_Parallel_CPU

open Viterbi_Cons

type msg =
    | Data of int * int
    | End

let viterbi (observSpace: int[]) stateCount (startProbs : double[])  (observSeq : int[]) (transitionProbs : double[][]) (emissionProbs : double[][]) =
    let hiddenStateSeq = [|0..observSeq.Length - 1|]
    let z = [|0..observSeq.Length - 1|]

    let tableMax = [|for i in 0..stateCount - 1 -> 
                       [|for j in 0..observSeq.Length - 1 ->
                           if j = 0
                           then startProbs.[i] * emissionProbs.[i].[observSeq.[0]]
                           else 0.0|] |]
    let tableArgMax = Array.init stateCount (fun _ -> Array.zeroCreate observSeq.Length)

    let is = ref false
    let agent num = 
        MailboxProcessor.Start(fun inbox ->
            let rec loop n =
                async {
                       let! msg = inbox.Receive()
                       match msg with
                       |Data (i, j) ->
                            let (maxVal, maxNum) = Viterbi_Cons.maxFun [|for k in 0..stateCount - 1 -> tableMax.[k].[i - 1] * transitionProbs.[k].[j] * emissionProbs.[j].[observSeq.[i]]|]
                            tableMax.[j].[i] <- maxVal
                            tableArgMax.[j].[i] <- maxNum
                            return! loop n
                       |End -> is := true 
                       }
            loop 0)
    
    let streams = [|for i in 0..stateCount - 1 -> agent i|]

    for i in 1..observSeq.Length - 1 do
        for j in 0..stateCount - 1 do
            (streams.[j]).Post (Data (i, j))
    
    for i in 0..stateCount - 1 do
        (streams.[i]).Post End
    while not !is do()

    z.[observSeq.Length - 1] <- Array.maxBy (fun k -> tableMax.[k].[observSeq.Length - 1]) [|0..stateCount - 1|]
    hiddenStateSeq.[observSeq.Length - 1] <- z.[observSeq.Length - 1]
    for i in 1..(observSeq.Length - 1) do
        z.[observSeq.Length - i - 1] <- tableArgMax.[z.[observSeq.Length - i]].[observSeq.Length - i]
        hiddenStateSeq.[observSeq.Length - i - 1] <- z.[observSeq.Length - i - 1]
    hiddenStateSeq

    
