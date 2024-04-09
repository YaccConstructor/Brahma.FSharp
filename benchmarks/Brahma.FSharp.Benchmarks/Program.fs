open BenchmarkDotNet.Configs
open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Running
open Brahma.FSharp.Benchmarks.SetArguments
open Perfolizer.Mathematics.OutlierDetection
open Brahma.FSharp.Benchmarks
open Brahma.FSharp.Benchmarks.Ilgpu
open Brahma.FSharp.Benchmarks.Fscl

[<EntryPoint>]
let main argv =
    let benchmarks = BenchmarkSwitcher [|
        typeof<BenchmarkSetArgs>
        // alloc
        typeof<Concrete.BrahmaAllocIntBenchmark>
        typeof<Concrete.BrahmaAllocStructOfIntInt64Benchmark>
        typeof<Concrete.BrahmaAllocValueOptionOfIntBenchmark>
        typeof<Concrete.BrahmaAllocBoolBenchmark>

        typeof<Concrete.IlgpuAllocIntBenchmark>
        typeof<Concrete.IlgpuAllocStructOfIntInt64Benchmark>
        typeof<Concrete.IlgpuAllocValueOptionOfIntBenchmark>

        // write
        typeof<Concrete.BrahmaToDeviceIntBenchmark>
        typeof<Concrete.BrahmaToDeviceStructOfIntInt64Benchmark>
        typeof<Concrete.BrahmaToDeviceValueOptionOfIntBenchmark>
        typeof<Concrete.BrahmaToDeviceBoolBenchmark>

        typeof<Concrete.IlgpuToDeviceIntBenchmark>
        typeof<Concrete.IlgpuToDeviceStructOfIntInt64Benchmark>
        typeof<Concrete.IlgpuToDeviceValueOptionOfIntBenchmark>

        typeof<Concrete.FsclToDeviceIntBenchmark>
        typeof<Concrete.FsclToDeviceStructOfIntInt64Benchmark>

        // read
        typeof<Concrete.BrahmaToHostIntBenchmark>
        typeof<Concrete.BrahmaToHostStructOfIntInt64Benchmark>
        typeof<Concrete.BrahmaToHostValueOptionOfIntBenchmark>
        typeof<Concrete.BrahmaToHostBoolBenchmark>

        typeof<Concrete.IlgpuToHostIntBenchmark>
        typeof<Concrete.IlgpuToHostStructOfIntInt64Benchmark>
        typeof<Concrete.IlgpuToHostValueOptionOfIntBenchmark>

        // atomic
        typeof<BrahmaNativeAtomicBenchmarks>
        typeof<BrahmaSpinlockAtomicBenchmarks>
        typeof<IlgpuNativeAtomicBenchmarks>
        typeof<IlgpuSpinlockAtomicBenchmarks>
    |]

    let config =
        DefaultConfig.Instance
            .AddJob(
                Job.Dry
                    .WithWarmupCount(3)
                    .WithIterationCount(10)
                    .WithInvocationCount(3)
                    .WithOutlierMode(OutlierMode.RemoveUpper)
            )

    benchmarks.Run(argv, config) |> ignore

    0
