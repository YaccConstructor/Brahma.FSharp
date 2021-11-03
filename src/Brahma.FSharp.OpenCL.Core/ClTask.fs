namespace Brahma.FSharp.OpenCL

open FSharp.Quotations

type ClTask<'a> = ClTask of (ClContext -> 'a)

type ClTaskBuilder() =
    let runComputation (ClTask f) env = f env

    member this.Bind(x, f) =
        ClTask <| fun env ->
            let x' = runComputation x env
            runComputation (f x') env

    member this.Return(x) =
        ClTask <| fun _ ->
            x

    member this.ReturnFrom(x) =
        x

    member this.Zero() =
        this.Return(())

    member this.Combine(m1, m2) =
        this.Bind(m1, (fun () -> m2))

    member this.Delay(rest) =
        this.Bind(this.Zero(), (fun () -> rest ()))

    member this.Run(m) = m

    member this.TryWith(ClTask body, handler) =
        ClTask <| fun env ->
            try
                body env
            with
            | e ->
                let (ClTask handlerBody) = handler e
                handlerBody env

    member this.TryFinally(ClTask body, finalizer) =
        ClTask <| fun env ->
            try
                body env
            finally
                finalizer ()

    member this.Using(disposableRes: #System.IDisposable, f) =
        ClTask <| fun env ->
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

    let ask = ClTask id

    let runSync (context: ClContext) (ClTask f) =
        let res = f context
        context.CommandQueue.PostAndReply <| MsgNotifyMe
        res

    // TODO mb swith to manual threads or smth like this
    let inParallel (tasks: seq<ClTask<'a>>) = opencl {
        let! ctx = ask

        ctx.CommandQueue.PostAndReply <| Msg.MsgNotifyMe

        let syncMsgs = Msg.CreateBarrierMessages (Seq.length tasks)
        let ctxs = Array.create (Seq.length tasks) (ctx.WithNewCommandQueue())

        return
            tasks
            |> Seq.mapi
                (fun i task ->
                    opencl {
                        let! ctx = ask
                        let! result = task
                        ctx.CommandQueue.Post <| syncMsgs.[i]
                        return result
                    }
                    |> fun task -> async { return runComputation task <| ctx.WithNewCommandQueue() }
                )
            |> Async.Parallel
            |> Async.RunSynchronously
    }

[<AutoOpen>]
module ClTaskOpened =
    let runCommand (command: Expr<'range -> 'a>) (binder: ('range -> 'a) -> unit) : ClTask<unit> =
        opencl {
            let! ctx = ClTask.ask

            let kernel = ctx.CreateClKernel command

            ctx.CommandQueue.Post <| MsgSetArguments(fun () -> binder kernel.ArgumentsSetter)
            ctx.CommandQueue.Post <| Msg.CreateRunMsg<_, _>(kernel)
            kernel.ReleaseBuffers()
        }

    let runKernel (kernel: ClKernel<'range, 'a>) (processor: MailboxProcessor<Msg>) (binder: ('range -> 'a) -> unit) : ClTask<unit> =
        opencl {
            processor.Post <| MsgSetArguments(fun () -> binder kernel.ArgumentsSetter)
            processor.Post <| Msg.CreateRunMsg<_, _>(kernel)
            kernel.ReleaseBuffers()
        }
