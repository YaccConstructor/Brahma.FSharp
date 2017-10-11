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
            commandQueue.Dispose()
            provider.Dispose()
            provider.CloseAllBuffers()
            constant outArr
        member __.Delay(f)      = f ()
    

    
module tests =
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
        
    let Indus = new BrahmaBuilder(actcontext)
    let computation1 =
             Indus 
                 { 
                                 
                     let! c = ArrayGPU.Reverse a
                     let! d = ArrayGPU.Map <@ fun a -> a + 1 @> c
                     return d
                   } 
        
    //Now we have to unwrap the value which we get from the computation
    //To do that we use the run function from the Reader module with the same context that we use in the computation
    let test1 = Reader.run actcontext computation1
    let printresult result = printfn "result=%A" result
    printresult test1
    //We can see, that it works just as expected
    //We get the result1 equil to [|17; 23; 9; 8; 6|]

    let y = 1
    
    let provider2 =
                try  ComputeProvider.Create(platformName, deviceType)
                with 
                | ex -> failwith ex.Message
    let mutable commandQueue2 = new CommandQueue(provider1, provider1.Devices |> Seq.head) 
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
    let Indus2=BrahmaBuilder(actcontext2)

    let computation2 =
             Indus2 
                 { 
                     //let a = [|5; 7; 8; 22; 16|]             
                     let! c = ArrayGPU.Reverse a
                     //let! d = c1 c 
                     return c
                   } 
    let test2 = Reader.run actcontext computation2
      //If we try to compose it with the same computation we get an error about the wrong worksize
    


 