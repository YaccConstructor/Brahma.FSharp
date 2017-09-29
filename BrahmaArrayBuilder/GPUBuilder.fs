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

    // basic operations

    let run dep (rm : ReaderM<_,_>) =
        rm dep

    let constant (c : 'c) : ReaderM<_,'c> =
        fun _ -> c

    // lifting of functions and state

    let lift1 (f : 'd -> 'a -> 'out)
              : 'a -> ReaderM<'d, 'out> =
        fun a dep -> f dep a

    let lift2 (f : 'd -> 'a -> 'b -> 'out)
              : 'a -> 'b -> ReaderM<'d, 'out> =
        fun a b dep -> f dep a b

    let lift3 (f : 'd -> 'a -> 'b -> 'c -> 'out)
              : 'a -> 'b -> 'c -> ReaderM<'d, 'out> =
        fun a b c dep -> f dep a b c

    let liftDep (proj : 'd2 -> 'd1) 
                (rm : ReaderM<'d1, 'output>) 
                : ReaderM<'d2, 'output> =
        proj >> rm
            
    // functor

    let fmap (f : 'a -> 'b) 
             (g : 'c -> 'a) 
             : ('c -> 'b) =
        g >> f

    let map (f : 'a -> 'b) 
            (rm : ReaderM<'d, 'a>) 
            : ReaderM<'d,'b> =
        rm >> f

    let (<?>) = map

    // applicative-functor
        
    let apply (f : ReaderM<'d, 'a->'b>)
              (rm : ReaderM<'d, 'a>)
              : ReaderM<'d, 'b> =
        fun dep ->
            let f' = run dep f
            let a  = run dep rm
            f' a

    let (<*>) = apply

    // monad

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
    type Context<'a> = Reader<context, 'a>

    and BrahmaBuilder (length: int) =
   
        //defining context
        let platformName = "NVIDIA*"
        let deviceType = DeviceType.Default        
        let provider =
                try  ComputeProvider.Create(platformName, deviceType)
                with 
                | ex -> failwith ex.Message
        let mutable commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head) 
        let localWorkSize = 
            let lws, ex = OpenCL.Net.Cl.GetDeviceInfo(provider.Devices |> Seq.head, OpenCL.Net.DeviceInfo.MaxWorkGroupSize)
            let maxWorkSize = int <| lws.CastTo<uint64>()
            if length <= maxWorkSize then length
            else 
                let mutable l = maxWorkSize
                while (length % l <> 0) do 
                    l <- l - 1
                l
        let (actcontext: context) = (provider, commandQueue, length, localWorkSize)
        

        member __.Bind(m, f)    = m >>= f
        member __.Return (outArr: array<_>) =
            let _ = commandQueue.Add(outArr.ToHost provider).Finish()
            commandQueue.Dispose()
            provider.Dispose()
            provider.CloseAllBuffers()
            constant outArr
        member __.Delay(f)      = f ()
    



    let Indus = new BrahmaBuilder(5)

    let test1 =
      Indus 
         { 
            let a = [|5, 7, 8, 22, 16|]
            let! b = ArrayGPU.Reverse a 
//            let! c = ArrayGPU.Map <@
//                                     fun (rng:_1D) (a:array<_>) ->
//                                     let i = rng.GlobalID0
//                                     if (i >= 0) && (i <= 3)
//                                     then a.[i] <- 0 @> b
            return b
         } 




 