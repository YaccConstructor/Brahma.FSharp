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

let tableToLine row col (a : 'T [][]) = 
    Array.init (row * col) (fun i -> a.[i / col].[i % col])

let lineToTable row col (a : array<_>) = 
    Array2D.init row col (fun i j -> a.[j + i * row])

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
            fun (r:_2D) rowCount rowLen (tableMax : array<_>) (tableArgMax : array<_>) stateCount (transitionProbs : array<_>) (emissionProbs : array<_>) (observSeq : array<_>) -> 
                let col = r.GlobalID0
                let row = r.GlobalID1
                let mutable mx = 0.0
                let mutable num = 0
                for i in 1..rowLen - 1 do
                    mx <- 0.0
                    num <- 0
                    for k in 0..rowCount - 1 do
                        let el = tableMax.[k * rowLen + (i - 1)] * transitionProbs.[k * rowCount + row] * emissionProbs.[row * rowLen + observSeq.[i]]
                        if mx < el
                        then
                            mx <- el
                            num <- k
                    tableMax.[row * rowCount + i] <- mx
                    tableArgMax.[row * rowCount + i] <- num
                    
        @>

    let kernel, kernelPrepare, kernelRun = provider.Compile command
    let d =(new _2D(stateCount, observSeq.Length))
    kernelPrepare d stateCount observSeq.Length tableMax tableArgMax stateCount transitionProbs emissionProbs observSeq
    let _ = commandQueue.Add(kernelRun()).Finish()            
    let _ = commandQueue.Add(tableMax.ToHost provider).Finish()
    let _ = commandQueue.Add(tableArgMax.ToHost provider).Finish()
    commandQueue.Dispose()
    provider.Dispose()
    provider.CloseAllBuffers()
    (lineToTable stateCount observSeq.Length tableMax, lineToTable stateCount observSeq.Length tableArgMax)


let viterbi (observSpace: int[]) stateCount (startProbs : double[])  (observSeq : int[]) (transitionProbs : double[][]) (emissionProbs : double[][]) =
    let hiddenStateSeq = [|0..observSeq.Length - 1|]
    let z = [|0..observSeq.Length - 1|]

    let tableMax = [|for i in 0..stateCount - 1 -> 
                       [|for j in 0..observSeq.Length - 1 ->
                           if j = 0
                           then startProbs.[i] * emissionProbs.[i].[observSeq.[0]]
                           else 0.0|] |]
    let tableArgMax = Array.init stateCount (fun _ -> Array.zeroCreate observSeq.Length)
    
    match (Parallel (tableToLine stateCount observSeq.Length tableMax) (tableToLine stateCount observSeq.Length tableArgMax) stateCount (tableToLine stateCount stateCount transitionProbs) (tableToLine stateCount observSpace.Length emissionProbs) observSeq) with
    |(max, argMax) ->
        for i in 1..observSeq.Length - 1 do
            for j in 0..stateCount - 1 do
                tableMax.[j].[i] <- max.[j,i]
                tableArgMax.[j].[i] <- argMax.[j,i]

    z.[observSeq.Length - 1] <- Array.maxBy (fun k -> tableMax.[k].[observSeq.Length - 1]) [|0..stateCount - 1|]
    hiddenStateSeq.[observSeq.Length - 1] <- z.[observSeq.Length - 1]
    for i in 1..(observSeq.Length - 1) do
        z.[observSeq.Length - i - 1] <- tableArgMax.[z.[observSeq.Length - i]].[observSeq.Length - i]
        hiddenStateSeq.[observSeq.Length - i - 1] <- z.[observSeq.Length - i - 1]
    hiddenStateSeq