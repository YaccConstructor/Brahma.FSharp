namespace BrahmaBuilder

open OpenCL.Net
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Extensions 

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

module GPUBuilder =
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
        let provider = prov actcontext
        let mutable commandQueue = CQ actcontext
        let length = len actcontext 
        let localWorkSize = WS actcontext
        
        member __.Bind(m, f)    = m >>= f
        member __.Yield (outArr: array<_>) =
            let _ = commandQueue.Add(outArr.ToHost provider).Finish()
            constant outArr
        member __.Zero = constant None
        member __.Return (outArr: array<_>) =
            let _ = commandQueue.Add(outArr.ToHost provider).Finish()
            commandQueue.Dispose()
            provider.Dispose()
            provider.CloseAllBuffers()
            constant outArr
        member __.Delay(f)      = f ()