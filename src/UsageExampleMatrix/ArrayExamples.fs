module ArrayExamples

open Brahma.Helpers
open OpenCL.Net
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Extensions

//Following function generates an array filled with Fibonacci numbers
let Fib length =
    let localWorkSize = 20
    let platformName = "NVIDIA*"
    let deviceType = DeviceType.Default
    
    let provider =
        try  ComputeProvider.Create(platformName, deviceType)
        with 
        | ex -> failwith ex.Message
   
    let commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)    

    let command = 
        <@
            fun (rng:_1D) (a:array<_>) ->
                for i in 2 .. length - 1 do
                    a.[i] <- a.[i - 1] + a.[i - 2]
        @>

    let a = Array.init (length) (fun i -> int64(1))

    let kernel, kernelPrepare, kernelRun = provider.Compile command
    let d =(new _1D(length, localWorkSize))
    kernelPrepare d a

    let _ = commandQueue.Add(kernelRun())
    let _ = commandQueue.Add(a.ToHost provider).Finish()
    commandQueue.Dispose()
    provider.Dispose()
    provider.CloseAllBuffers()

    a

//Following function makes elements of the array arr to equal 0 in range of [firstElement, lastElement]
let Nulify (arr: array<_>) length firstElement lastElement =
    let localWorkSize = 20
    let platformName = "NVIDIA*"
    let deviceType = DeviceType.Default
    
    let provider =
        try  ComputeProvider.Create(platformName, deviceType)
        with 
        | ex -> failwith ex.Message
   
    let commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)    

    let command = 
        <@
            fun (rng:_1D) (a:array<_>) ->
                let i = rng.GlobalID0
                if (i >= firstElement) && (i <= lastElement)
                then a.[i] <- 0
        @>

    let kernel, kernelPrepare, kernelRun = provider.Compile command
    let d =(new _1D(length, localWorkSize))
    kernelPrepare d arr

    let _ = commandQueue.Add(kernelRun())
    let _ = commandQueue.Add(arr.ToHost provider).Finish()
    commandQueue.Dispose()
    provider.Dispose()
    provider.CloseAllBuffers()

    arr

//Following function reverts the array arr
let Reflect (arr: array<_>) length =
    let localWorkSize = 20
    let platformName = "NVIDIA*"
    let deviceType = DeviceType.Default
    
    let provider =
        try  ComputeProvider.Create(platformName, deviceType)
        with 
        | ex -> failwith ex.Message
   
    let commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)    

    let command = 
        <@
            fun (rng:_1D) (a:array<_>) ->
                let i = rng.GlobalID0
                let buff = a.[i] 
                a.[i] <- a.[length - i]
                a.[length - i] <- buff
        @>

    let kernel, kernelPrepare, kernelRun = provider.Compile command
    let d =(new _1D(length, localWorkSize))
    kernelPrepare d arr

    let _ = commandQueue.Add(kernelRun())
    let _ = commandQueue.Add(arr.ToHost provider).Finish()
    commandQueue.Dispose()
    provider.Dispose()
    provider.CloseAllBuffers()

    arr