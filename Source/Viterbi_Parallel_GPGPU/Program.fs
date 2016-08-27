module Viterbi_Parallel_GPGPU

open Brahma
open Brahma.Helpers
open OpenCL.Net
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Extensions
open System
open System.Threading
open Viterbi_Cons

let tableToLine row col (a : 'T [][]) = 
    Array.init (row * col) (fun i -> a.[i / col].[i % col])

let lineToTable row col (a : array<_>) = 
    Array2D.init row col (fun i j -> a.[i * col + j])

let Parallel (tableMax : array<_>) (tableArgMax : array<_>) stateCount (transitionProbs : array<_>) (emissionProbs : array<_>) (observSeq : array<_>) =
    let platformName = "*"
    let deviceType = DeviceType.Default
    let provider =
        try  ComputeProvider.Create(platformName, deviceType)
        with 
        | ex -> failwith ex.Message
    let mutable commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)
    let command = 
        <@
            fun (r:_1D) rowCount rowLen (tableMax : array<_>) (tableArgMax : array<_>) stateCount (transitionProbs : array<_>) (emissionProbs : array<_>) (observSeq : array<_>) -> 
                let row = r.GlobalID0
                let mutable mx = 0.0
                let mutable num = 0
                let mutable buf = 0
                for i in 1..rowLen - 1 do
                    buf <- 0
                    mx <- 0.0
                    num <- 0
                    for k in 0..rowCount - 1 do
                        let el = tableMax.[k * rowLen + (i - 1)] * transitionProbs.[k * rowCount + row] * emissionProbs.[row * rowLen + observSeq.[i]]
                        if mx < el
                        then
                            mx <- el
                            num <- k
                    tableMax.[row * rowLen + i] <- mx
                    tableArgMax.[row * rowLen + i] <! num
                    while (buf < 100000000) do(buf <- buf + 1)
        @>

    let kernel, kernelPrepare, kernelRun = provider.Compile command
    let d =(new _1D(stateCount) )
    kernelPrepare d stateCount observSeq.Length tableMax tableArgMax stateCount transitionProbs emissionProbs observSeq
    let _ = commandQueue.Add(kernelRun()).Finish()            
    let _ = commandQueue.Add(tableMax.ToHost provider).Finish()
    let _ = commandQueue.Add(tableArgMax.ToHost provider).Finish()
    commandQueue.Dispose()
    provider.Dispose()
    provider.CloseAllBuffers()
    (lineToTable stateCount observSeq.Length tableMax, lineToTable stateCount observSeq.Length tableArgMax)

let viterbiGpgpu (observSpace: int[]) (tableMax : double[][]) (tableArgMax : int[][]) stateCount  (observSeq : int[]) (transitionProbs : double[][]) (emissionProbs : double[][]) =
    let (max, argMax) = Parallel (tableToLine stateCount observSeq.Length tableMax) (tableToLine stateCount observSeq.Length tableArgMax) stateCount (tableToLine stateCount stateCount transitionProbs) (tableToLine stateCount observSpace.Length emissionProbs) observSeq
    for i in 1..observSeq.Length - 1 do
        for j in 0..stateCount - 1 do
            tableMax.[j].[i] <- max.[j,i]
            tableArgMax.[j].[i] <- argMax.[j,i]
    (tableMax, tableArgMax)

let viterbi (observSpace: int[]) stateCount (startProbs : double[])  (observSeq : int[]) (transitionProbs : double[][]) (emissionProbs : double[][]) =
    Viterbi_Cons.mainPart viterbiGpgpu (observSpace: int[]) stateCount (startProbs : double[])  (observSeq : int[]) (transitionProbs : double[][]) (emissionProbs : double[][])