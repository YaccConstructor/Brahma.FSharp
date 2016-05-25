module ArrayGPU

open Brahma.Helpers
open OpenCL.Net
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Extensions 

let init (inArr: array<_>) = 
    let platformName = "*"
    let deviceType = DeviceType.Default
    let provider = 
        try ComputeProvider.Create(platformName, deviceType)
        with
        | ex -> failwith ex.Message
    
    let getLocalWorkSize (inArr: array<_>) = 
            let lws, ex = OpenCL.Net.Cl.GetDeviceInfo(provider.Devices |> Seq.head, OpenCL.Net.DeviceInfo.MaxWorkGroupSize)
            let maxWorkSize = int <| lws.CastTo<uint64>()
            if inArr.Length <= maxWorkSize then inArr.Length
            else 
                let mutable l = maxWorkSize
                while (inArr.Length % l <> 0) do 
                    l <- l - 1
                l
    let length = inArr.Length
    if length = 0 then failwith "Array is empty"
    let localWorkSize = getLocalWorkSize inArr
    let mutable commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)
    let t = provider, commandQueue, length, localWorkSize
    t    

let Map func (inArr: array<_>) ((provider: ComputeProvider), (commandQueue: CommandQueue), length, localWorkSize) =
    let command = 
         <@
            fun (rng: _1D) (a: array<_>) (b: array<_>) ->
                let r = rng.GlobalID0 
                b.[r] <- (%func) a.[r]                                 
         @>
    let outArr = Array.zeroCreate length  
    let kernel, kernelPrepare, kernelRun = provider.Compile command 
    let d = new _1D(length, localWorkSize)
    kernelPrepare d inArr outArr 
    let _ = commandQueue.Add(kernelRun()) 
    outArr

let Mapi func (inArr: array<_>) ((provider: ComputeProvider), (commandQueue: CommandQueue), length, localWorkSize) =  
    let command = 
        <@
            fun (rng: _1D) (a: array<_>) (b: array<_>) ->
                let r = rng.GlobalID0 
                let i = r
                b.[r] <- (%func) i a.[i]                               
        @>
    let outArr = Array.zeroCreate length  
    let kernel, kernelPrepare, kernelRun = provider.Compile command 
    let d = new _1D(length, localWorkSize)
    kernelPrepare d inArr outArr 
    let _ = commandQueue.Add(kernelRun()) 
    outArr

let Map2 func (inArr1: array<_>) (inArr2: array<_>) ((provider: ComputeProvider), (commandQueue: CommandQueue), length, localWorkSize) = 
    if inArr1.Length <> inArr2.Length then failwith "Arrays must have the same lengths"
    let command = 
        <@
            fun (rng: _1D) (a1: array<_>) (a2: array<_>) (b: array<_>) ->
                let r = rng.GlobalID0 
                b.[r] <- (%func) a1.[r] a2.[r]                               
        @>
    let outArr = Array.zeroCreate length  
    let kernel, kernelPrepare, kernelRun = provider.Compile command 
    let d = new _1D(length, localWorkSize)
    kernelPrepare d inArr1 inArr2 outArr 
    let _ = commandQueue.Add(kernelRun()) 
    outArr

let Reverse (inArr: array<_>) ((provider: ComputeProvider), (commandQueue: CommandQueue), length, localWorkSize) = 
    let outArr = Array.zeroCreate length   
    let command =
        <@
            fun (rng: _1D) (a: array<_>) (b: array<_>) length  ->
                let r = rng.GlobalID0 
                b.[length - r - 1] <- a.[r]                               
        @>
    let kernel, kernelPrepare, kernelRun = provider.Compile command
    let d = new _1D(length, localWorkSize)    
    kernelPrepare d inArr outArr length 
    let _ = commandQueue.Add(kernelRun()).Finish()
    outArr

let getResult (outArr: array<_>) ((provider: ComputeProvider), (commandQueue: CommandQueue), length, localWorkSize) = 
    let _ = commandQueue.Add(outArr.ToHost provider).Finish()
    commandQueue.Dispose()
    provider.Dispose()
    provider.CloseAllBuffers()
    outArr
















