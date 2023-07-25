namespace Brahma.FSharp.Benchmarks.Ilgpu

open System
open Brahma.FSharp.Benchmarks
open BenchmarkDotNet.Attributes
open ILGPU
open ILGPU.AtomicOperations
open ILGPU.Runtime
open ILGPU.Runtime.Cuda

[<AbstractClass>]
type IlgpuAtomicBenchamrks() =
    inherit AtomicBenchamrks()

    member val Program = Unchecked.defaultof<Action<Index1D, VariableView<int>>> with get, set

    member val Cell = Unchecked.defaultof<MemoryBuffer1D<int, Stride1D.Dense>> with get, set

    member val Accelerator =
        let context = Context.CreateDefault()
        context.CreateCudaAccelerator(0)

    abstract Command: Action<Index1D, VariableView<int>>

    [<GlobalSetup>]
    member this.CompileProgram() =
        this.Program <- this.Accelerator.LoadAutoGroupedStreamKernel<Index1D, VariableView<int>>(this.Command)

    [<IterationSetup>]
    member this.AllocCellOnDevice() =
       this.Cell <- this.Accelerator.Allocate1D<int>(1L)
       this.Cell.MemSetToZero()

    abstract RunProgram : unit -> unit
    default this.RunProgram() =
        this.Program.Invoke(Index1D this.GlobalWorkSize, this.Cell.View.VariableView(Index1D 0))
        this.Accelerator.Synchronize()

    [<IterationCleanup>]
    member this.CleanCell() =
        this.Cell.Dispose()

type IlgpuNativeAtomicBenchmarks() =
    inherit IlgpuAtomicBenchamrks()

    override this.Command =
        Action<Index1D, VariableView<int>>(fun index dataView ->
            Atomic.Add(&dataView.Value, 1) |> ignore
        )

    [<Benchmark(Baseline = true)>]
    override this.RunProgram() = base.RunProgram()

[<Struct>]
type AddOp =
    interface IAtomicOperation<int> with
        member this.Operation(current, value) = current + value

[<Struct>]
type CmpXchOp =
    interface ICompareExchangeOperation<int> with
        member this.CompareExchange(target, compare, value) = Atomic.CompareExchange(&target, compare, value);
        member this.IsSame(left, right) = left = right

type IlgpuSpinlockAtomicBenchmarks() =
    inherit IlgpuAtomicBenchamrks()

    override this.Command =
        Action<Index1D, VariableView<int>>(fun index dataView ->
            Atomic.MakeAtomic(
                &dataView.Value,
                1,
                AddOp(),
                CmpXchOp()
            ) |> ignore
        )

    [<Benchmark>]
    override this.RunProgram() = base.RunProgram()
