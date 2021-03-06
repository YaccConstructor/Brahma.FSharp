module Brahma.FSharp.OpenCL.WorkflowBuilder.Basic

open FSharp.Quotations

open Brahma.FSharp.OpenCL.WorkflowBuilder.Evaluation
open Brahma.FSharp.OpenCL.Extensions
open Brahma.FSharp.OpenCL.Core

let opencl = OpenCLEvaluationBuilder()

let getEvaluationContext : OpenCLEvaluation<OpenCLEvaluationContext> =
    OpenCLEvaluation <| fun x -> x

type OpenCLEvaluationContext with
    member this.RunSync (OpenCLEvaluation f) =
        let res = f this
        this.CommandQueue.Finish() |> ignore
        res

    member this.RunAsync (OpenCLEvaluation f) =
        fun () -> this.RunSync <| OpenCLEvaluation f


/// If array is associated with gpu memory, then ToHost
/// transfers it to host memory and returns a ordinary F# array.
/// Otherwise ToHost simply returns the array passed to it, like
/// the identity function.
let ToHost (xs : array<'a>) : OpenCLEvaluation<array<'a>> =
    opencl {
        let! ctx = getEvaluationContext

        if ctx.Provider.AutoconfiguredBuffers.ContainsKey xs then
            ctx.CommandQueue.Add(xs.ToHost ctx.Provider) |> ignore

        return xs
    }

let RunCommand (command : Expr<'range -> 'a>) (binder : ('range -> 'a) -> unit) : OpenCLEvaluation<unit> =
    opencl {
        let! ctx = getEvaluationContext

        let (_, kernelPrepare, kernelRun) =
            if not ctx.IsCachingEnabled then
                ctx.Provider.Compile command
            else
                match ctx.CompilingCache.TryGetValue <| ExprWrapper command.Raw with
                | true, (kernel, kernelPrepare, kernelRun) ->
                    unbox<Brahma.OpenCL.Kernel<'range>> kernel,
                    unbox<'range -> 'a> kernelPrepare,
                    unbox<unit -> Brahma.OpenCL.Commands.Run<'range>> kernelRun
                | false, _ ->
                    let (kernel, kernelPrepare, kernelRun) = ctx.Provider.Compile command
                    ctx.CompilingCache.Add(ExprWrapper command.Raw, (box kernel, box kernelPrepare, box kernelRun))
                    kernel, kernelPrepare, kernelRun

        binder kernelPrepare
        ctx.CommandQueue.Add(kernelRun()) |> ignore
    }
