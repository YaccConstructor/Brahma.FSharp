namespace Brahma.FSharp

open FSharp.Quotations

type ClTask<'a> = ClTask of (RuntimeContext -> 'a)

[<AutoOpen>]
module internal ClTaskBuilder =
    let inline runComputation (ClTask f) env = f env

// TODO inlineiflambda
/// Represents a computation expression for OpenCL computations.
type ClTaskBuilder() =
    member inline this.Bind(x, f) =
        ClTask
        <| fun env ->
            let x' = runComputation x env
            runComputation (f x') env

    member inline this.Return(x) = ClTask <| fun _ -> x

    member inline this.ReturnFrom(x) = x

    member inline this.Zero() = this.Return(())

    member inline this.Combine(m1, m2) = this.Bind(m1, (fun () -> m2))

    member inline this.Delay(rest) =
        this.Bind(this.Zero(), (fun () -> rest ()))

    member inline this.Run(m) = m

    member this.TryWith(ClTask body, handler) =
        ClTask
        <| fun env ->
            try
                body env
            with e ->
                let (ClTask handlerBody) = handler e
                handlerBody env

    member this.TryFinally(ClTask body, finalizer) =
        ClTask
        <| fun env ->
            try
                body env
            finally
                finalizer ()

    member this.Using(disposableRes: #System.IDisposable, f) =
        ClTask
        <| fun env ->
            try
                runComputation (this.Delay(fun () -> f disposableRes)) env
            finally
                env.CommandQueue.Post <| Msg.CreateFreeMsg(disposableRes)

    member this.While(cond, body) =
        if not (cond ()) then
            this.Zero()
        else
            this.Combine(this.Run(body), this.Delay(fun () -> this.While(cond, body)))

    member this.For(xs: seq<'T>, f) =
        this.Bind(
            this.Return(xs.GetEnumerator()),
            fun en -> this.While((fun () -> en.MoveNext()), this.Delay(fun () -> f en.Current))
        )

[<AutoOpen>]
module ClTaskImpl =
    let opencl = ClTaskBuilder()

    let (>>=) x f = opencl.Bind(x, f)

module ClTask =
    let private runComputation (ClTask f) env = f env

    /// Returns computation context.
    let ask = ClTask id

    /// Returns runtime options.
    let runtimeOptions = ask >>= fun env -> opencl.Return env.RuntimeOptions

    /// Creates computation with specified options.
    let withOptions (g: RuntimeOptions -> RuntimeOptions) (ClTask f) =
        ask
        >>= fun env -> opencl.Return(f <| env.WithRuntimeOptions(g env.RuntimeOptions))

    /// Runs computation with specified runtime context.
    let runSync (context: RuntimeContext) (ClTask f) =
        let res = f context
        context.CommandQueue.PostAndReply <| MsgNotifyMe
        res

    // TODO implement
    // let startSync (ClTask f) =
    //     let context = Device.getFirstAppropriateDevice
    //     let res = f context
    //     context.CommandQueue.PostAndReply <| MsgNotifyMe
    //     res

    // NOTE maybe switch to manual threads
    // TODO check if it is really parallel
    /// Runs computations in parallel.
    let inParallel (tasks: seq<ClTask<'a>>) =
        opencl {
            let! ctx = ask

            ctx.CommandQueue.PostAndReply <| Msg.MsgNotifyMe

            let syncMsgs = Msg.CreateBarrierMessages(Seq.length tasks)
            let ctxs = Array.create (Seq.length tasks) (ctx.WithNewCommandQueue())

            return
                tasks
                |> Seq.mapi (fun i task ->
                    opencl {
                        let! ctx = ask
                        let! result = task
                        ctx.CommandQueue.Post <| syncMsgs.[i]
                        return result
                    }
                    |> fun task -> async { return runComputation task <| ctx.WithNewCommandQueue() })
                |> Async.Parallel
                |> Async.RunSynchronously
        }

[<AutoOpen>]
module ClTaskOpened =
    /// Runs specified pre-compiled program.
    let runProgram (program: ClProgram<'range, 'a>) (binder: ('range -> 'a) -> unit) : ClTask<unit> =
        opencl {
            let! ctx = ClTask.ask

            let kernel = ClKernel(program)

            ctx.CommandQueue.Post <| MsgSetArguments(fun () -> binder kernel.KernelFunc)
            ctx.CommandQueue.Post <| Msg.CreateRunMsg<_, _>(kernel)
            kernel.ReleaseInternalBuffers(ctx.CommandQueue)
        }

    /// Compiles specified raw kernel and runs using specified binder.
    let runCommand (command: Expr<'range -> 'a>) (binder: ('range -> 'a) -> unit) : ClTask<unit> =
        opencl {
            let! ctx = ClTask.ask

            let program = ClProgram(ctx.ClContext, command)

            do! runProgram program binder
        }
