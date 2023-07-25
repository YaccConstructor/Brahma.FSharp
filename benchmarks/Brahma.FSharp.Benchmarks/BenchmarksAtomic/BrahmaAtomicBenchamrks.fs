namespace Brahma.FSharp.Benchmarks

open BenchmarkDotNet.Attributes
open Brahma.FSharp
open Brahma.FSharp.OpenCL.Shared
open FSharp.Quotations

[<AbstractClass>]
type BrahmaAtomicBenchamrks() =
    inherit AtomicBenchamrks()

    member val Program = Unchecked.defaultof<ClProgram<Range1D, int clcell -> unit>> with get, set

    member val Cell = Unchecked.defaultof<int clcell> with get, set

    [<ParamsSource("AvaliableContextsProvider")>]
    member val Context = Unchecked.defaultof<RuntimeContext> with get, set

    abstract Command: Expr<Range1D -> int clcell -> unit>

    [<GlobalSetup>]
    member this.CompileProgram() =
        this.Program <- this.Context.ClContext.Compile(this.Command)

    [<IterationSetup>]
    member this.AllocCellOnDevice() =
       this.Cell <-
            opencl {
                return! ClCell.alloc<int> ()
            }
            |> ClTask.runSync this.Context

    abstract RunProgram : unit -> unit
    default this.RunProgram() =
        opencl {
            do! runProgram this.Program <| fun kernel ->
                kernel
                <| Range1D.CreateValid(this.GlobalWorkSize, this.WgSize)
                <| this.Cell
        }
        |> ClTask.runSync this.Context

    [<IterationCleanup>]
    member this.CleanCell() =
        this.Cell.Dispose()

    static member AvaliableContextsProvider =
        ClDevice.GetAvailableDevices(Platform.Nvidia)
        |> Seq.map RuntimeContext

type BrahmaNativeAtomicBenchmarks() =
    inherit BrahmaAtomicBenchamrks()

    override this.Command =
        <@
            fun (range: Range1D) (acc: int clcell) ->
                atomic (+) acc.Value 1 |> ignore
        @>

    [<Benchmark(Baseline = true)>]
    override this.RunProgram() = base.RunProgram()

type BrahmaSpinlockAtomicBenchmarks() =
    inherit BrahmaAtomicBenchamrks()

    override this.Command =
        <@
            fun (range: Range1D) (acc: int clcell) ->
                atomic (fun x -> x + 1) acc.Value |> ignore
        @>

    [<Benchmark>]
    override this.RunProgram() = base.RunProgram()
