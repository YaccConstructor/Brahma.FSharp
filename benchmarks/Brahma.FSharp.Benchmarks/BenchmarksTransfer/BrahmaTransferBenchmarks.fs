﻿namespace Brahma.FSharp.Benchmarks

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
        // TODO Точно ли оно дождется завершения?
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
        // TODO Точно ли оно дождется завершения?
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

    // TODO мб можно в глобал сетап вынести
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
        // TODO Точно ли оно дождется завершения?
        opencl {
            return ClArray.toHost this.DeviceArray
        }
        |> ClTask.runSync this.Context
        |> ignore

    [<IterationCleanup>]
    member this.CleanBuffers() =
        this.DeviceArray.Dispose()

