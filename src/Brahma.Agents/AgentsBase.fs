namespace Brahma.FSharp.Agents

open Brahma.Helpers
open OpenCL.Net
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Extensions


type GpuConfig =
    val Name: string
    val Workers: int
    new (n,w) = {Name=n; Workers=w}

type WorkerConfig =
    val AdditionalBuffsNum: uint32
    val GpuCommandQueue: CommandQueue
    val GPUProvider: ComputeProvider
    new (bNum,queue,provider) = {AdditionalBuffsNum = bNum; GpuCommandQueue = queue; GPUProvider = provider}

type msg<'data,'res> =
    | Die of AsyncReplyChannel<unit>
    | Process of 'data*('res -> unit)
    | PostProcess of 'res
    | Fill of 'data*(Option<'data> -> unit)
    | InitBuffers of array<'data>*AsyncReplyChannel<array<'data>>
    | Get of AsyncReplyChannel<Option<'data>>
    | Enq of 'data

type Reader<'d> (fillF:'d -> Option<'d>) as this =
    let isTurnedOff = ref false    
    let inner =
        MailboxProcessor.Start(fun inbox ->
            let rec loop n =
                async {
                        let! msg = inbox.Receive()
                        match msg with
                        | Die ch ->
                            ch.Reply()
                            return ()
                        | Fill (x,cont) ->
                            let filled = fillF x
                            cont filled
                            if filled.IsNone
                            then this.Die()
                            else return! loop n
                        | x -> 
                            printfn "unexpected message for reader: %A" x
                            return! loop n }
            loop 0)
    
    member this.Read(a, cont) = inner.Post(Fill(a, cont))
    member this.Die() = inner.PostAndReply((fun reply -> Die reply), timeout = 20000)    

type Worker<'d,'r>(f: 'd -> 'r) =
    let inner =
        MailboxProcessor.Start(fun inbox ->
            let rec loop n =
                async { let! msg = inbox.Receive()
                        match msg with
                        | Die ch ->
                            ch.Reply()
                            return ()                            
                        | Process (x,continuation) ->
                            let r = f x
                            continuation r
                            return! loop n
                        | x -> 
                            printfn "unexpected message for Worker: %A" x
                            return! loop n }
            loop 0)
 
    member this.Process(a, continuation) = inner.Post(Process(a,continuation))
    member this.Die() = inner.PostAndReply((fun reply -> Die reply), timeout = 20000)

type DataManager<'d>(readers:array<Reader<'d>>) =
    let dataToProcess = new System.Collections.Concurrent.ConcurrentQueue<Option<'d>>()
    let dataToFill = new System.Collections.Generic.Queue<_>()
    let dataIsEnd = ref false
    let inner =
        MailboxProcessor.Start(fun inbox ->
            let rec loop n =
                async { 
                        let cnt = ref 3
                        while dataToFill.Count > 0 && !cnt > 0 do
                            decr cnt                            
                            let b = dataToFill.Dequeue()
                            if not <| !dataIsEnd
                            then readers.[0].Read(b
                                , fun a ->                                     
                                    dataToProcess.Enqueue a
                                    dataIsEnd := Option.isNone a)
                        if inbox.CurrentQueueLength > 0
                        then
                            let! msg = inbox.Receive()
                            match msg with
                            | Die ch ->
                                if !dataIsEnd 
                                then
                                    ch.Reply()
                                    return ()
                                else 
                                    inbox.Post(Die ch)
                                    return! loop n
                            | InitBuffers (bufs,ch) ->
                                ch.Reply bufs
                                bufs |> Array.iter dataToFill.Enqueue                                
                                return! loop n
                            | Get(ch) -> 
                                let s,r = dataToProcess.TryDequeue()
                                if s
                                then 
                                    if r.IsNone then dataIsEnd := true
                                    ch.Reply r
                                elif not !dataIsEnd
                                then inbox.Post(Get ch)
                                else ch.Reply None
                                return! loop n
                            | Enq b -> 
                                dataToFill.Enqueue b
                                return! loop n
                            | x ->  
                                printfn "Unexpected message for Worker: %A" x
                                return! loop n
                            else return! loop n }
            loop 0)
 
    member this.InitBuffers(bufs) = inner.PostAndReply((fun reply -> InitBuffers(bufs,reply)), timeout = 20000)
    member this.GetData() = 
        inner.PostAndReply((fun reply -> Get reply), timeout = 20000)
    member this.Enq(b) = inner.Post(Enq b)
    member this.Die() = inner.PostAndReply((fun reply -> Die reply), timeout = 20000)

type Master<'d,'r,'fr>(workers:array<Worker<'d,'r>>, fill: 'd -> Option<'d>, bufs:ResizeArray<'d>, postProcessF:Option<'r->'fr>) =        
    
    let isDataEnd = ref false
    let reader = new Reader<_>(fill)
    let mutable isEnd = false
            
    let dataManager = new DataManager<'d>([|reader|])

    let postprocessor =
        postProcessF |> Option.map(fun f ->new Worker<'r,'fr>(f))        

    let bufers = dataManager.InitBuffers(bufs.ToArray())
    let freeWorkers = new System.Collections.Concurrent.ConcurrentQueue<_>(workers)
    let inner =
        MailboxProcessor.Start(fun inbox ->
            let rec loop n =
                async {
                        if not freeWorkers.IsEmpty
                        then 
                            let success,w = freeWorkers.TryDequeue()
                            if success
                            then
                                let b = dataManager.GetData()
                                if b.IsSome
                                then
                                    w.Process
                                        (b.Value
                                        , fun a -> 
                                            postprocessor |> Option.iter (fun p -> p.Process(a,fun _ -> ()))
                                            freeWorkers.Enqueue w
                                            dataManager.Enq b.Value)
                                else 
                                    isDataEnd := true
                        if inbox.CurrentQueueLength > 0
                        then
                            let! msg = inbox.Receive()
                            match msg with
                            | Die ch ->
                                dataManager.Die()                                
                                workers |> Array.iter (fun w -> w.Die())
                                match postprocessor with Some p -> p.Die() | None -> ()
                                isEnd <- true
                                ch.Reply()
                                return ()
                            | x ->
                                printfn "unexpected message for Worker: %A" x
                                return! loop n 
                        else return! loop n}
            loop 0)

    member this.Die() = inner.PostAndReply(fun reply -> Die reply)
    member this.IsDataEnd() = !isDataEnd
