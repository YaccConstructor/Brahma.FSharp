module Viterbi.Parallel

open Viterbi.Cons

type msg =
    | Data of int * int
    | Cont
    | End

let viterbiCpu (observSpace: int[]) (tableMax : double[][]) (tableArgMax : int[][]) stateCount  (observSeq : int[]) (transitionProbs : double[][]) (emissionProbs : double[][]) =
    let agent num = 
        MailboxProcessor.Start(fun inbox ->
            let rec loop n =
                async {
                       let! msg = inbox.Receive()
                       match msg with
                       | Data (i, j) ->
                            let (maxVal, maxNum) = Viterbi.Cons.maxFun [|for k in 0..stateCount - 1 -> tableMax.[k].[i - 1] * transitionProbs.[k].[j] * emissionProbs.[j].[observSeq.[i]]|]
                            tableMax.[j].[i] <- maxVal
                            tableArgMax.[j].[i] <- maxNum
                            return! loop n
                       | Cont ->
                            return! loop n
                       | End -> 
                            do()
                       }
            loop 0)
    
    let streams = [|for i in 0..stateCount - 1 -> agent i|]

    let x buf =
        for j in 0..stateCount - 1 do 
            buf := !buf + streams.[j].CurrentQueueLength
        buf
        
    for i in 1..observSeq.Length - 1 do
        for j in 0..stateCount - 1 do
            (streams.[j]).Post (Data (i, j))
        for j in 0..stateCount - 1 do
            (streams.[j]).Post (Cont)
        while !(x (ref 0)) <> 0 do()

     
    for i in 0..stateCount - 1 do
        (streams.[i]).Post End
    (tableMax, tableArgMax)

let viterbi (observSpace: int[]) stateCount (startProbs : double[])  (observSeq : int[]) (transitionProbs : double[][]) (emissionProbs : double[][]) =
    Viterbi.Cons.mainPart viterbiCpu (observSpace: int[]) stateCount (startProbs : double[])  (observSeq : int[]) (transitionProbs : double[][]) (emissionProbs : double[][])
