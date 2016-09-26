module Viterbi.Cons

let maxFun (arr : array<double>) =
    let mutable mx = 0.0
    let mutable num = 0
    for i in 0..arr.Length - 1 do
        if mx < arr.[i]
        then 
            mx <- arr.[i]
            num <- i
    (mx, num)    

let mainPart tableFun (observSpace: int[]) stateCount (startProbs : double[])  (observSeq : int[]) (transitionProbs : double[][]) (emissionProbs : double[][]) =
    let hiddenStateSeq = [|0..observSeq.Length - 1|]
    let z = [|0..observSeq.Length - 1|]
    let tableMax = [|for i in 0..stateCount - 1 -> 
                       [|for j in 0..observSeq.Length - 1 ->
                           if j = 0
                           then startProbs.[i] * emissionProbs.[i].[observSeq.[0]]
                           else 0.0|] |]
    let tableArgMax = Array.init stateCount (fun _ -> Array.zeroCreate observSeq.Length)

    (tableMax, tableArgMax) = tableFun observSpace tableMax tableArgMax stateCount  observSeq transitionProbs emissionProbs |> ignore

    z.[observSeq.Length - 1] <- Array.maxBy (fun k -> tableMax.[k].[observSeq.Length - 1]) [|0..stateCount - 1|]
    hiddenStateSeq.[observSeq.Length - 1] <- z.[observSeq.Length - 1]
    for i in 1..(observSeq.Length - 1) do
        z.[observSeq.Length - i - 1] <- tableArgMax.[z.[observSeq.Length - i]].[observSeq.Length - i]
        hiddenStateSeq.[observSeq.Length - i - 1] <- z.[observSeq.Length - i - 1]
    hiddenStateSeq

let viterbiCons (observSpace: int[]) (tableMax : double[][]) (tableArgMax : int[][]) stateCount  (observSeq : int[]) (transitionProbs : double[][]) (emissionProbs : double[][]) =
    for i in 1..observSeq.Length - 1 do
        for j in 0..stateCount - 1 do
            let (maxVal, maxNum) = maxFun [|for k in 0..stateCount - 1 -> tableMax.[k].[i - 1] * transitionProbs.[k].[j] * emissionProbs.[j].[observSeq.[i]]|]
            tableMax.[j].[i] <- maxVal
            tableArgMax.[j].[i] <- maxNum
    (tableMax, tableArgMax)

let viterbi (observSpace: int[]) stateCount (startProbs : double[])  (observSeq : int[]) (transitionProbs : double[][]) (emissionProbs : double[][]) =
    mainPart (viterbiCons) (observSpace: int[]) stateCount (startProbs : double[])  (observSeq : int[]) (transitionProbs : double[][]) (emissionProbs : double[][])