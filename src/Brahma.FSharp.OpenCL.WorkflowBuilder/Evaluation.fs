module Brahma.FSharp.OpenCL.WorkflowBuilder.Evaluation

open Brahma.OpenCL
open OpenCL.Net
open System.Collections.Generic
open FSharp.Quotations

exception EmptyDevicesException of string

type internal ExprWrapper(e: Expr) =
    override this.GetHashCode() =
        e.ToString().GetHashCode()

    override this.Equals(other: obj) =
        (not << isNull) other &&
        this.GetType() = other.GetType() &&
        e.ToString() = (other :?> ExprWrapper).ToString()

    override this.ToString() =
        e.ToString()

type OpenCLEvaluationContext(provider: ComputeProvider, ?device_id: int) =
    let devices = provider.Devices |> Seq.toArray
    let device_id = defaultArg device_id 0
    do
        if device_id >= devices.Length then
            raise (EmptyDevicesException <|
                   sprintf "Provider:\n%Ahas not device with id %i." provider device_id)

    let device = devices.[device_id]
    let command_queue = new Brahma.OpenCL.CommandQueue(provider, device)

    member this.Provider = provider

    member this.Device = device

    member this.CommandQueue = command_queue

    member val IsCachingEnabled = false with get, set

    member val internal CompilingCache = Dictionary<ExprWrapper, obj * obj * obj>() with get

    new (?platform_name, ?device_type: DeviceType) =
        let platform_name = defaultArg platform_name "*"
        let device_type = defaultArg device_type DeviceType.Default
        let provider = ComputeProvider.Create(platform_name, device_type)
        OpenCLEvaluationContext(provider)

type OpenCLEvaluation<'a> =
    OpenCLEvaluation of (OpenCLEvaluationContext -> 'a)

let private runEvaluation (OpenCLEvaluation f) = f

type OpenCLEvaluationBuilder() =
    abstract member Return : 'a -> OpenCLEvaluation<'a>

    abstract member ReturnFrom : OpenCLEvaluation<'a> -> OpenCLEvaluation<'a>

    abstract member Zero : unit -> OpenCLEvaluation<unit>

    abstract member Bind : OpenCLEvaluation<'a> * ('a -> OpenCLEvaluation<'b>) -> OpenCLEvaluation<'b>

    abstract member Combine : OpenCLEvaluation<unit> * OpenCLEvaluation<'a> -> OpenCLEvaluation<'a>

    abstract member Delay : (unit -> OpenCLEvaluation<'a>) -> OpenCLEvaluation<'a>

    abstract member While : (unit -> bool) * OpenCLEvaluation<unit> -> OpenCLEvaluation<unit>

    abstract member For : seq<'elem> * ('elem -> OpenCLEvaluation<unit>) -> OpenCLEvaluation<unit>

    abstract member TryWith : OpenCLEvaluation<'a> * (exn -> OpenCLEvaluation<'a>) -> OpenCLEvaluation<'a>

    default this.Return x =
        OpenCLEvaluation <| fun _ -> x

    default this.ReturnFrom m = m

    default this.Zero () =
        this.Return ()

    default this.Bind (m, k) =
        OpenCLEvaluation <| fun env ->
            let res = runEvaluation m env
            runEvaluation <| k res <| env

    default this.Combine (m1, m2) =
        OpenCLEvaluation <| fun env ->
            runEvaluation m1 env
            runEvaluation m2 env

    default this.Delay (rest) =
        OpenCLEvaluation <| fun env ->
            runEvaluation (rest()) env

    default this.While (predicate, body) =
        OpenCLEvaluation <| fun env ->
            while predicate() do
                runEvaluation body env

    default this.For (elems, f_body) =
        OpenCLEvaluation <| fun env ->
            for elem in elems do
                runEvaluation (f_body elem) env

    default this.TryWith (tryBlock, handler) =
        OpenCLEvaluation <| fun env ->
            try
                runEvaluation tryBlock env
            with
            | e ->
                runEvaluation (handler e) env
