module Viterbi.GPGPU

open OpenCL.Net
open Brahma
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Brahma.FSharp.OpenCL.Extensions
open Brahma.FSharp.OpenCL.Translator
open Microsoft.FSharp.Quotations
open System
open System.Threading
open Viterbi.Cons

let tableToLine fs sc (a : 'T [][]) = 
    //Array.init (fs * sc) (fun i -> a.[i / sc].[i % sc])
    Array.concat a

let lineToTable fs sc (a : array<_>) = 
    Array2D.init fs sc (fun i j -> a.[i * sc + j])

let Parallel (tableMax : array<_>) (tableArgMax : array<_>) stateCount (transitionProbs : array<_>) (emissionProbs : array<_>) (observSeq : array<_>) obsSpaceLen =
    let platformName = "*"
    let deviceType = DeviceType.Default
    let provider =
        try  ComputeProvider.Create(platformName, deviceType)
        with 
        | ex -> failwith ex.Message
    let mutable commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)
    let command = 
        <@
            fun (r:_1D) stateCount obsSeqLen obsSpaceLen (i : array<_>) (tableMax : array<_>) (tableArgMax : array<_>) (transitionProbs : array<_>) (emissionProbs : array<_>) (observSeq : array<_>) -> 
                let row = r.GlobalID0
                let mutable mx = 0.0
                let mutable num = 0
                for k in 0..stateCount - 1 do
                    let el = tableMax.[k * obsSeqLen + (i.[0] - 1)] * transitionProbs.[k * stateCount + row] * emissionProbs.[row * obsSpaceLen + observSeq.[i.[0]]]
                    if mx < el
                    then
                        mx <- el
                        num <- k
                tableMax.[row * obsSeqLen + i.[0]] <- mx
                tableArgMax.[row * obsSeqLen + i.[0]] <- num
        @>

    let kernel, kernelPrepare, kernelRun = provider.Compile command
    let d =(new _1D(stateCount, 1))
    let index = [|1|]
    kernelPrepare d stateCount observSeq.Length obsSpaceLen index tableMax tableArgMax transitionProbs emissionProbs observSeq
    for i in 1..observSeq.Length do
        index.[0] <- i
        let _ = commandQueue.Add(index.ToGpu provider).Finish()
        commandQueue.Add(kernelRun()).Finish()
        
    let _ = commandQueue.Add(tableMax.ToHost provider).Finish()
    let _ = commandQueue.Add(tableArgMax.ToHost provider).Finish()       
    
    commandQueue.Dispose()
    provider.Dispose()
    provider.CloseAllBuffers()
    (lineToTable stateCount observSeq.Length tableMax, lineToTable stateCount observSeq.Length tableArgMax)

let viterbiGpgpu (observSpace: int[]) (tableMax : double[][]) (tableArgMax : int[][]) stateCount  (observSeq : int[]) (transitionProbs : double[][]) (emissionProbs : double[][]) =
    let (max, argMax) = Parallel (Array.concat tableMax) (Array.concat tableArgMax) stateCount (Array.concat transitionProbs) (Array.concat emissionProbs) observSeq observSpace.Length

    for i in 0..stateCount - 1 do
        for j in 1..observSeq.Length - 1 do
            tableMax.[i].[j] <- max.[i,j]
            tableArgMax.[i].[j] <- argMax.[i,j]

    (tableMax, tableArgMax)

let viterbi (observSpace: int[]) stateCount (startProbs : double[])  (observSeq : int[]) (transitionProbs : double[][]) (emissionProbs : double[][]) =
    Viterbi.Cons.mainPart viterbiGpgpu (observSpace: int[]) stateCount (startProbs : double[])  (observSeq : int[]) (transitionProbs : double[][]) (emissionProbs : double[][])