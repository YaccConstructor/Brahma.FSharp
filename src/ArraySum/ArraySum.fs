module ArraySum

open Brahma.Helpers
open OpenCL.Net
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Extensions

let random = new System.Random()
        

let length = 200
//random.Next(10)
let baseArr = Array.init length (fun i -> i)

let cpuArr = Array.copy baseArr
let gpuArr = Array.copy baseArr

let Main gpuArr =

    let platformName = "*"
    
    let localWorkSize = 20   
    let deviceType = DeviceType.Default

    let provider =
        try  ComputeProvider.Create(platformName, deviceType)
        with 
        | ex -> failwith ex.Message

    printfn "Using %A" provider

    let commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)    

    let command = 
        <@
            fun (rng:_1D) (a:array<_>) ->
                let r = rng.GlobalID0                
                a.[r] <- a.[r] + 1
        @>


    let kernel, kernelPrepare, kernelRun = provider.Compile command
    let d = new _1D(length,localWorkSize)    
    kernelPrepare d gpuArr
    let go () =
        let _ = commandQueue.Add(kernelRun())//.Finish()    
        //let _ = commandQueue.Add(gpuArr.ToHost provider).Finish()
        printfn "Result= %A" gpuArr
    
    go()
    //go()
    //go()
    //let _ = commandQueue.Add(kernelRun()).Finish()
    //printfn "done."
    //Array.iteri(fun i _ -> gpuArr.[i] <- 0 ) gpuArr
    //let _ = commandQueue.Add(gpuArr.ToGpu provider).Finish()
    let _ = commandQueue.Add(gpuArr.ToHost provider).Finish()
    printfn "Result= %A" gpuArr
    
    commandQueue.Dispose()
    provider.Dispose()
    provider.CloseAllBuffers()

    ignore (System.Console.Read())

do Main gpuArr