namespace Brahma.FSharp.Benchmarks

open BenchmarkDotNet.Attributes
open Brahma.FSharp
open Brahma.FSharp.OpenCL.Shared

[<AbstractClass>]
type BrahmaTransferBenchmarks<'a>() =
    inherit TransferBenchmarks<'a>()

    member val DeviceArray = Unchecked.defaultof<'a clarray> with get, set

    [<ParamsSource("AvaliableContextsProvider")>]
    member val Context = Unchecked.defaultof<RuntimeContext> with get, set

    static member AvaliableContextsProvider =
        ClDevice.GetAvailableDevices(Platform.Nvidia)
        |> Seq.map RuntimeContext

type BrahmaAllocBenchmarks<'a>(?flags: ClMemFlags) =
    inherit BrahmaTransferBenchmarks<'a>()

    let flags = defaultArg flags ClMemFlags.DefaultIfNoData

    [<Benchmark>]
    member this.AllocArrayToDevice() =
        this.DeviceArray <-
            opencl {
                let! array = ClArray.allocWithFlags<'a> this.ArrayLength flags
                return array
            }
            |> ClTask.runSync this.Context

    [<IterationCleanup>]
    member this.CleanBuffers() =
        this.DeviceArray.Dispose()

type BrahmaToDeviceBenchmarks<'a>(?flags: ClMemFlags) =
    inherit BrahmaTransferBenchmarks<'a>()

    let flags = defaultArg flags ClMemFlags.DefaultIfData

    [<Benchmark>]
    member this.WriteArrayToDevice() =
        this.DeviceArray <-
            opencl {
                let! array = ClArray.toDeviceWithFlags this.HostArray flags
                return array
            }
            |> ClTask.runSync this.Context

    [<IterationCleanup>]
    member this.CleanBuffers() =
        this.DeviceArray.Dispose()

type BrahmaToHostBenchmarks<'a>(?flags: ClMemFlags) =
    inherit BrahmaTransferBenchmarks<'a>()

    let flags = defaultArg flags ClMemFlags.DefaultIfData

    [<IterationSetup>]
    member this.WriteArrayToDevice() =
        this.DeviceArray <-
            opencl {
                let! array = ClArray.toDeviceWithFlags this.HostArray flags
                return array
            }
            |> ClTask.runSync this.Context

    [<Benchmark>]
    member this.ReadArrayFromDevice() =
        opencl {
            return! ClArray.toHost this.DeviceArray
        }
        |> ClTask.runSync this.Context

    [<IterationCleanup>]
    member this.CleanBuffers() =
        this.DeviceArray.Dispose()

module Concrete =
    type BrahmaAllocIntBenchmark() = inherit BrahmaAllocBenchmarks<int>()
    type BrahmaAllocStructOfIntInt64Benchmark() = inherit BrahmaAllocBenchmarks<StructOfIntInt64>()
    type BrahmaAllocValueOptionOfIntBenchmark() = inherit BrahmaAllocBenchmarks<ValueOption<int>>()
    type BrahmaAllocBoolBenchmark() = inherit BrahmaAllocBenchmarks<bool>()

    type BrahmaToDeviceIntBenchmark() = inherit BrahmaToDeviceBenchmarks<int>()
    type BrahmaToDeviceStructOfIntInt64Benchmark() = inherit BrahmaToDeviceBenchmarks<StructOfIntInt64>()
    type BrahmaToDeviceValueOptionOfIntBenchmark() = inherit BrahmaToDeviceBenchmarks<ValueOption<int>>()
    type BrahmaToDeviceBoolBenchmark() = inherit BrahmaToDeviceBenchmarks<bool>()

    type BrahmaToHostIntBenchmark() = inherit BrahmaToHostBenchmarks<int>()
    type BrahmaToHostStructOfIntInt64Benchmark() = inherit BrahmaToHostBenchmarks<StructOfIntInt64>()
    type BrahmaToHostValueOptionOfIntBenchmark() = inherit BrahmaToHostBenchmarks<ValueOption<int>>()
    type BrahmaToHostBoolBenchmark() = inherit BrahmaToHostBenchmarks<bool>()
