namespace Brahma.FSharp

open System.Drawing
open System.Threading
open System.Threading.Channels
open System.Threading.Tasks
open Brahma.FSharp.OpenCL.Translator
open OpenCL.Net
open System
open System.Runtime.InteropServices

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

/// Provides the ability to create multiple command queues.
type CommandQueueProvider private (device, context, translator: FSQuotationToOpenCLTranslator, __: unit) =
    let finish queue =
        let error = Cl.Finish(queue)

        if error <> ErrorCode.Success then
            raise <| Cl.Exception error

    let handleFree (free: IFreeCrate) =
        { new IFreeCrateEvaluator with
            member this.Eval crate = crate.Source.Dispose()
        }
        |> free.Apply

    let handleToGPU queue (toGpu: IToGPUCrate) =
        { new IToGPUCrateEvaluator with
            member this.Eval crate =
                let eventID = ref Unchecked.defaultof<Event>

                let mem = crate.Destination.Memory
                let elementSize = crate.Destination.ElementSize

                let error =
                    Cl.EnqueueWriteBuffer(
                        queue,
                        mem,
                        Bool.False,
                        IntPtr(0),
                        IntPtr(crate.Destination.Length * elementSize),
                        crate.Source,
                        0u,
                        null,
                        eventID
                    )

                if error <> ErrorCode.Success then
                    raise (Cl.Exception error)
        }
        |> toGpu.Apply

    let handleToHost queue (toHost: IToHostCrate) =
        { new IToHostCrateEvaluator with
            member this.Eval(crate: ToHost<'a>) =
                let eventID = ref Unchecked.defaultof<Event>
                let clMem = crate.Source.Memory
                let marshaller = translator.Marshaller

                let finishRead error =
                    if error <> ErrorCode.Success then
                        raise (Cl.Exception error)

                    finish queue

                if marshaller.IsBlittable typeof<'a> then
                    Cl.EnqueueReadBuffer(
                        queue,
                        clMem,
                        Bool.False,
                        IntPtr(0),
                        IntPtr(crate.Source.Length * crate.Source.ElementSize),
                        crate.Destination,
                        0u,
                        null,
                        eventID
                    )
                    |> finishRead
                else
                    let size = crate.Destination.Length * marshaller.GetTypePacking(typeof<'a>).Size
                    let hostMem = Marshal.AllocHGlobal size

                    Cl.EnqueueReadBuffer(
                        queue,
                        clMem,
                        Bool.False,
                        IntPtr(0),
                        IntPtr(crate.Source.Length * crate.Source.ElementSize),
                        hostMem,
                        0u,
                        null,
                        eventID
                    )
                    |> finishRead

                    marshaller.ReadFromUnmanaged(hostMem, crate.Destination)
                    Marshal.FreeHGlobal(hostMem)

                match crate.ReplyChannel with
                | Some ch -> ch.Reply crate.Destination
                | None -> ()
        }
        |> toHost.Apply

    let handleRun queue (run: IRunCrate) =
        { new IRunCrateEvaluator with
            member this.Eval crate =
                let range = crate.Kernel.NDRange
                let workDim = uint32 range.Dimensions
                let eventID = ref Unchecked.defaultof<Event>

                let error =
                    Cl.EnqueueNDRangeKernel(
                        queue,
                        crate.Kernel.Kernel,
                        workDim,
                        null,
                        range.GlobalWorkSize,
                        range.LocalWorkSize,
                        0u,
                        null,
                        eventID
                    )

                if error <> ErrorCode.Success then
                    raise (Cl.Exception error)
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
                    finish commandQueue
                    itIsFirstNonqueueMsg <- false

                handleFree crate

            | MsgSetArguments setterFunc ->
                if itIsFirstNonqueueMsg then
                    finish commandQueue
                    itIsFirstNonqueueMsg <- false

                setterFunc ()

            | MsgBarrier syncObject ->
                itIsFirstNonqueueMsg <- true
                finish commandQueue
                syncObject.ImReady()

                while not <| syncObject.CanContinue() do
                    ()

        let processor = DeviceCommandQueue((fun () -> finish commandQueue), msgHandler)

        // TODO rethink error handling?
        //processor.Error.AddHandler(Handler<_>(fun _ -> raise))

        processor
