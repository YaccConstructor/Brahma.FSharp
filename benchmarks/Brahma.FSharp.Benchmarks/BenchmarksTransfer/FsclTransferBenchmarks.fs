namespace Brahma.FSharp.Benchmarks.Fscl

open BenchmarkDotNet.Attributes
open Brahma.FSharp.Benchmarks
open FSCL.Runtime
open FSCL.Language

type FsclToDeviceBenchmarks<'a>() =
    inherit TransferBenchmarks<'a>()

    let mutable array = Unchecked.defaultof<'a[]>

    [<Benchmark>]
    member this.WriteArrayToDevice() =
        array <-
            <@
                this.HostArray
                |> Array.map (fun x -> x)
            @>.Run()

module Concrete =
    type FsclToDeviceIntBenchmark() = inherit FsclToDeviceBenchmarks<int>()
    type FsclToDeviceStructOfIntInt64Benchmark() = inherit FsclToDeviceBenchmarks<StructOfIntInt64>()
