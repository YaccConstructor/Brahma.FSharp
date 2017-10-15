open OpenCL.Net
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Extensions 
open ArrayGPU

type Reader<'environment,'a> = Reader of ('environment -> 'a)
type ReaderM<'d,'out> = 
    'd -> 'out

module Reader =

    let run dep (rm : ReaderM<_,_>) =
        rm dep

    let constant (c : 'c) : ReaderM<_,'c> =
        fun _ -> c
    
    let bind (rm : ReaderM<'d, 'a>) 
             (f : 'a -> ReaderM<'d,'b>) 
             : ReaderM<'d, 'b> =
        fun dep ->
            f (rm dep) 
            |> run dep 

    let (>>=) = bind    

module BrahmaBuilder =
    open Reader
           
    type context = (ComputeProvider * CommandQueue * int * int)
    
    //We grant named access to the elements of the context tuple
    let prov(x:context) =
        match x with
            |(a,b,c,d) -> a
    let CQ (x:context) =
        match x with
            |(a,b,c,d) -> b
    let len(x:context) =
        match x with
            |(a,b,c,d) -> c
    let WS(x:context) =
        match x with
            |(a,b,c,d) -> d

    //Implementing a builder, using the methods from Reader Monad
    type BrahmaBuilder (actcontext: context) =     
        let mutable provider = prov actcontext
        let mutable commandQueue = CQ actcontext
        let mutable length = len actcontext 
        let mutable localWorkSize = WS actcontext
        
        member __.Bind(m, f)    = m >>= f
        member __.Return (outArr: array<_>) =
            let _ = commandQueue.Add(outArr.ToHost provider).Finish()
            constant outArr
        member __.Delay(f)      = f ()
    

    
module test1 =
    open BrahmaBuilder
    //defining context
    let platformName = "NVIDIA*"
    let deviceType = DeviceType.Default        
    let provider1 =
                try  ComputeProvider.Create(platformName, deviceType)
                with 
                | ex -> failwith ex.Message
    let mutable commandQueue1 = new CommandQueue(provider1, provider1.Devices |> Seq.head) 
    let length1 = 5
    
    let getLocalWorkSize1 (length) = 
            let lws, ex = OpenCL.Net.Cl.GetDeviceInfo(provider1.Devices |> Seq.head, OpenCL.Net.DeviceInfo.MaxWorkGroupSize)
            let maxWorkSize = int <| lws.CastTo<uint64>()
            if length <= maxWorkSize then length
            else 
                let mutable l = maxWorkSize
                while (length % l <> 0) do 
                    l <- l - 1
                l
    
    let localworksize1 = getLocalWorkSize1 length1
    

    let actcontext = provider1, commandQueue1, length1, localworksize1
       
    let a = [|5; 7; 8; 22; 16|] 
        
    let gpu = new BrahmaBuilder(actcontext)
    
     
                
  
    let computation1 =
             gpu 
                 { 
                                 
                     let! c = ArrayGPU.Reverse a
                     let! d =  gpu 
                                {                             
                                    let! e = ArrayGPU.Reverse a
                                    let! f = ArrayGPU.Map <@ fun a -> a + 1 @> e
                                    return f
                                } 
                     let! g = ArrayGPU.Map2 <@ fun a b -> a + b @> c d
                     return g
                   } 
  
    //Now we have to unwrap the value which we get from the computation
    //To do that we use the run function from the Reader module with the same context that we use in the computation
    let test1 = Reader.run actcontext computation1
    let printresult result = printfn "result=%A" result
    printresult test1
    //We can see, that it works just as expected
    //We get the result1 equil to [|17; 23; 9; 8; 6|]
    commandQueue1.Dispose()
    provider1.Dispose()
    provider1.CloseAllBuffers()
module test2 = 
    open BrahmaBuilder
    let platformName = "NVIDIA*"
    let deviceType = DeviceType.Default    
    let provider2 =
                try  ComputeProvider.Create(platformName, deviceType)
                with 
                | ex -> failwith ex.Message
    let mutable commandQueue2 = new CommandQueue(provider2, provider2.Devices |> Seq.head) 
    let length2 = 6
    
    let getLocalWorkSize2 (length) = 
            let lws, ex = OpenCL.Net.Cl.GetDeviceInfo(provider2.Devices |> Seq.head, OpenCL.Net.DeviceInfo.MaxWorkGroupSize)
            let maxWorkSize = int <| lws.CastTo<uint64>()
            if length <= maxWorkSize then length
            else 
                let mutable l = maxWorkSize
                while (length % l <> 0) do 
                    l <- l - 1
                l
    let localworksize2 = getLocalWorkSize2 length2
    

    let actcontext2 = provider2, commandQueue2, length2, localworksize2
    let gpu2=BrahmaBuilder(actcontext2)

    let computation2 =
             gpu2 
                 { 
                     let a = [|5; 7; 8; 22; 16|]             
                     let! c = ArrayGPU.Reverse a
                     let! d = ArrayGPU.Map <@ fun a -> a + 1 @> c
                     return d
                   } 
    let test2 = Reader.run actcontext2 computation2
      //If we try to compose it with the same computation we get an error about the wrong worksize
    let printresult result = printfn "result=%A" result
    printresult test2
    commandQueue2.Dispose()
    provider2.Dispose()
    provider2.CloseAllBuffers()
    let x = 1

 