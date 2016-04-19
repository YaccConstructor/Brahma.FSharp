module Viterbi_Cons

let viterbi (observSpace: int[]) (stateSpace: int[]) (startProbs : double[])  (observSeq : int[]) (transitionProbs : double[][]) (emissionProbs : double[][]) =
    let mutable X = [|0..observSeq.Length - 1|]
    let mutable Z = [|0..observSeq.Length - 1|]

    let T1 = [|for i in stateSpace -> 
                            [|for j in 0..observSeq.Length - 1 ->
                                    if j = 0
                                    then startProbs.[i] * emissionProbs.[i].[observSeq.[0]]
                                    else 0.0|]|]
    let T2 = [|for i in stateSpace -> 
                            [|for j in 0..observSeq.Length - 1 -> 0|]|]
    for i in 1..observSeq.Length - 1 do
        for j in stateSpace do
            T1.[j].[i] <- Array.max([|for k in stateSpace -> 
                                (T1.[k].[i - 1] * transitionProbs.[k].[j] * emissionProbs.[j].[observSeq.[i]])|])
            T2.[j].[i] <- Array.maxBy(fun k -> T1.[k].[i - 1] * transitionProbs.[k].[j] * emissionProbs.[j].[observSeq.[i]]) stateSpace
    Z.[observSeq.Length - 1] <- Array.maxBy(fun k -> T1.[k].[observSeq.Length - 1]) stateSpace
    X.[observSeq.Length - 1] <- stateSpace.[Z.[observSeq.Length - 1]]
    for i in 1..(observSeq.Length - 1) do
        Z.[observSeq.Length - i - 1] <- T2.[Z.[observSeq.Length - i]].[observSeq.Length - i]
        X.[observSeq.Length - i - 1] <- stateSpace.[Z.[observSeq.Length - i - 1]]
    X