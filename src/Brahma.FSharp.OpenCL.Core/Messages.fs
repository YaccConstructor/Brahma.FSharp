namespace Brahma.FSharp

open Brahma.FSharp.OpenCL.Shared

type Free(src: System.IDisposable) =
    member this.Source = src

type ToHost<'a>(src: IBuffer<'a>, dst: 'a[], ?replyChannel: AsyncReplyChannel<'a[]>) =
    member this.Destination = dst
    member this.Source = src
    member this.ReplyChannel = replyChannel

type ToGPU<'a>(src: 'a[], dst: IBuffer<'a>) =
    member this.Destination = dst
    member this.Source = src

type Run(kernel: IKernel) =
    member this.Kernel = kernel

type IRunCrate =
    abstract member Apply: IRunCrateEvaluator -> unit

and IRunCrateEvaluator =
    abstract member Eval: Run -> unit

type IToHostCrate =
    abstract member Apply: IToHostCrateEvaluator -> unit

and IToHostCrateEvaluator =
    abstract member Eval: ToHost<'a> -> unit

type IToGPUCrate =
    abstract member Apply: IToGPUCrateEvaluator -> unit

and IToGPUCrateEvaluator =
    abstract member Eval: ToGPU<'a> -> unit

type IFreeCrate =
    abstract member Apply: IFreeCrateEvaluator -> unit

and IFreeCrateEvaluator =
    abstract member Eval: Free -> unit

type SyncObject(numToWait: int) =
    let mutable canContinue = false

    let mutable counter = 0

    member this.ImReady() =
        lock this
        <| fun () ->
            counter <- counter + 1

            if counter = numToWait then
                canContinue <- true

    member this.CanContinue() = canContinue

type Msg =
    /// kik
    | MsgToHost of IToHostCrate
    | MsgToGPU of IToGPUCrate
    | MsgRun of IRunCrate
    | MsgFree of IFreeCrate
    | MsgSetArguments of (unit -> unit)
    | MsgNotifyMe of AsyncReplyChannel<unit>
    | MsgBarrier of SyncObject

    static member CreateToHostMsg<'a>(src, dst, ?ch) =
        { new IToHostCrate with
            member this.Apply evaluator = evaluator.Eval <| ToHost<'a>(src, dst, ?replyChannel = ch)
        }
        |> MsgToHost

    static member CreateToGPUMsg<'a>(src, dst) =
        { new IToGPUCrate with
            member this.Apply evaluator = evaluator.Eval <| ToGPU<'a>(src, dst)
        }
        |> MsgToGPU

    static member CreateFreeMsg<'a>(src) =
        { new IFreeCrate with
            member this.Apply evaluator = evaluator.Eval <| Free(src)
        }
        |> MsgFree

    static member CreateRunMsg<'TRange, 'a when 'TRange :> INDRange>(kernel) =
        { new IRunCrate with
            member this.Apply evaluator = evaluator.Eval <| Run(kernel)
        }
        |> MsgRun

    static member CreateBarrierMessages(numOfQueuesOnBarrier: int) =
        let s = SyncObject numOfQueuesOnBarrier
        Array.init numOfQueuesOnBarrier (fun _ -> MsgBarrier s)
