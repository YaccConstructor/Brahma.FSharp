namespace Brahma.FSharp

open System.Threading
open System.Threading.Channels
open System.Threading.Tasks
open Brahma.FSharp.OpenCL.Shared
open Brahma.FSharp.OpenCL.Translator
open OpenCL.Net
open System
open System.Runtime.InteropServices

module private QueueFunctions =
    let finish queue =
        let error = Cl.Finish(queue)

        if error <> ErrorCode.Success then
            raise <| Cl.Exception error

    let run queue (kernel: IKernel) =
        let range = kernel.NDRange
        let workDim = uint32 range.Dimensions
        let eventID = ref Unchecked.defaultof<Event>

        let error =
            Cl.EnqueueNDRangeKernel(queue, kernel.Kernel, workDim, null, range.GlobalWorkSize, range.LocalWorkSize, 0u, null, eventID)

        if error <> ErrorCode.Success then
            raise (Cl.Exception error)

    let toGPU queue blocking (source: array<'a>) (destination: IBuffer<'a>) =
        let eventID = ref Unchecked.defaultof<Event>

        let mem = destination.Memory
        let elementSize = destination.ElementSize
        let blocking = if blocking then Bool.True else Bool.False
        let error =
            Cl.EnqueueWriteBuffer(queue, mem, blocking, IntPtr(0), IntPtr(destination.Length * elementSize), source, 0u, null, eventID)

        if error <> ErrorCode.Success then
            raise (Cl.Exception error)

    let toHost queue (translator: FSQuotationToOpenCLTranslator) blocking (source: IBuffer<'a>) (destination: array<'a>) =
        let eventID = ref Unchecked.defaultof<Event>
        let clMem = source.Memory
        let marshaller = translator.Marshaller

        let finishRead error =

            if error <> ErrorCode.Success then
                raise (Cl.Exception error)

        let blocking = if blocking then Bool.True else Bool.False

        if marshaller.IsBlittable typeof<'a> then
            Cl.EnqueueReadBuffer(
                queue,
                clMem,
                blocking,
                IntPtr(0),
                IntPtr(source.Length * source.ElementSize),
                destination,
                0u,
                null,
                eventID
            )
            |> finishRead
        else
            let size = destination.Length * marshaller.GetTypePacking(typeof<'a>).Size
            let hostMem = Marshal.AllocHGlobal size

            Cl.EnqueueReadBuffer(queue, clMem, blocking, IntPtr(0), IntPtr(source.Length * source.ElementSize), hostMem, 0u, null, eventID)
            |> finishRead

            marshaller.ReadFromUnmanaged(hostMem, destination)
            Marshal.FreeHGlobal(hostMem)

type msg<'a> =
    | Regular of 'a
    | Synchronization of TaskCompletionSource

type DeviceCommandQueue<'message>(clQueueFinishFunction, messageHandler) =
    let inbox = Channel.CreateUnbounded<msg<'message>>()
    let cts = new CancellationTokenSource()

    let work (cancellationToken: CancellationToken) =
        task {
            try
                while not cancellationToken.IsCancellationRequested do
                    let! ok = inbox.Reader.WaitToReadAsync(cancellationToken)
                    let mutable read = ok

                    while read do
                        let ok, msg = inbox.Reader.TryRead()
                        read <- ok

                        if ok then
                            match msg with
                            | Regular a -> messageHandler a
                            | Synchronization c ->
                                clQueueFinishFunction ()
                                c.SetResult()
            finally
                ()
        }

    do work cts.Token |> ignore

    member this.Post msg =
        inbox.Writer.TryWrite(Regular msg) |> ignore

    member this.Synchronize() =
        let c = TaskCompletionSource()
        inbox.Writer.TryWrite(Synchronization c) |> ignore
        c.Task.GetAwaiter().GetResult()
        ()

    member this.Stop() = inbox.Writer.Complete()

type RawCommandQueue(device: Device, context: Context, translator: FSQuotationToOpenCLTranslator) =
    let commandQueue =
        let error = ref Unchecked.defaultof<ErrorCode>
        let props = CommandQueueProperties.None
        let queue = Cl.CreateCommandQueue(context, device, props, error)

        if error.Value <> ErrorCode.Success then
            raise <| Cl.Exception error.Value

        queue

    member this.Synchronize() = QueueFunctions.finish commandQueue

    member this.ToHost(source: IBuffer<'a>, destination: array<'a>, blocking) =
        QueueFunctions.toHost commandQueue translator blocking source destination

    member this.ToGPU(source: array<'a>, destination: IBuffer<'a>, blocking) =
        QueueFunctions.toGPU commandQueue blocking source destination

    member this.RunKernel(kernel: IKernel) = QueueFunctions.run commandQueue kernel

/// Provides the ability to create multiple command queues.
type CommandQueueProvider private (device, context, translator: FSQuotationToOpenCLTranslator, __: unit) =
    let handleFree (free: IFreeCrate) =
        { new IFreeCrateEvaluator with
            member this.Eval crate = crate.Source.Dispose()
        }
        |> free.Apply

    let handleToGPU queue (toGpu: IToGPUCrate) =
        { new IToGPUCrateEvaluator with
            member this.Eval crate =
                QueueFunctions.toGPU queue false crate.Source crate.Destination
        }
        |> toGpu.Apply

    let handleToHost queue (toHost: IToHostCrate) =
        { new IToHostCrateEvaluator with
            member this.Eval(crate: ToHost<'a>) =
                QueueFunctions.toHost queue translator true crate.Source crate.Destination

                match crate.ReplyChannel with
                | Some ch -> ch.Reply crate.Destination
                | None -> ()
        }
        |> toHost.Apply

    let handleRun queue (run: IRunCrate) =
        { new IRunCrateEvaluator with
            member this.Eval crate = QueueFunctions.run queue crate.Kernel
        }
        |> run.Apply

    /// <summary>
    /// Initializes a new instance of the <see cref="CommandQueueProvider"/> class with specified device, context and translator.
    /// </summary>
    new(device: Device, context: Context, translator: FSQuotationToOpenCLTranslator) = CommandQueueProvider(device, context, translator, ())

    /// <summary>
    /// Creates new command queue capable of handling messages of type <see cref="Msg"/>.
    /// </summary>
    member this.CreateQueue() =
        let commandQueue =
            let error = ref Unchecked.defaultof<ErrorCode>
            let props = CommandQueueProperties.None
            let queue = Cl.CreateCommandQueue(context, device, props, error)

            if error.Value <> ErrorCode.Success then
                raise <| Cl.Exception error.Value

            queue

        let mutable itIsFirstNonqueueMsg = true

        let msgHandler msg =
            match msg with
            | MsgToHost crate ->
                itIsFirstNonqueueMsg <- true
                handleToHost commandQueue crate

            | MsgToGPU crate ->
                itIsFirstNonqueueMsg <- true
                handleToGPU commandQueue crate

            | MsgRun crate ->
                itIsFirstNonqueueMsg <- true
                handleRun commandQueue crate

            | MsgFree crate ->
                if itIsFirstNonqueueMsg then
                    itIsFirstNonqueueMsg <- false

                handleFree crate

            | MsgSetArguments setterFunc ->
                if itIsFirstNonqueueMsg then
                    itIsFirstNonqueueMsg <- false

                setterFunc ()

            | MsgBarrier syncObject ->
                itIsFirstNonqueueMsg <- true
                QueueFunctions.finish commandQueue
                syncObject.ImReady()

                while not <| syncObject.CanContinue() do
                    ()

        let processor =
            DeviceCommandQueue((fun () -> QueueFunctions.finish commandQueue), msgHandler)

        // TODO rethink error handling?
        //processor.Error.AddHandler(Handler<_>(fun _ -> raise))

        processor
