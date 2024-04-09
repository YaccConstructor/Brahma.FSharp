module Brahma.FSharp.Benchmarks.SetArguments


open BenchmarkDotNet.Attributes
open Brahma.FSharp

type AllocationFlag =
        | DeviceOnly
        | HostInterop

type ClContext with
    member this.CreateClArrayWithSpecificAllocationMode(mode, array: 'a []) =
            match mode with
            | DeviceOnly ->
                this.CreateClArray(
                    array,
                    deviceAccessMode = DeviceAccessMode.ReadWrite,
                    hostAccessMode = HostAccessMode.NotAccessible,
                    allocationMode = AllocationMode.CopyHostPtr
                )
            | HostInterop ->
                this.CreateClArray(
                    array,
                    deviceAccessMode = DeviceAccessMode.ReadWrite,
                    hostAccessMode = HostAccessMode.ReadWrite,
                    allocationMode = AllocationMode.CopyHostPtr
                )


type ClArray<'a> with
        member this.FreeAndWait(q: DeviceCommandQueue<Msg>) =
            q.Post(Msg.CreateFreeMsg this)
            q.Synchronize()


[<IterationCount(20)>]
[<WarmupCount(10)>]
type BenchmarkSetArgs() =

    //let kernel = <@ fun (ndRange: Range1D) (inputArray1: ClArray<int>) (inputArray2: ClArray<int>) (inputArray3: ClArray<int>) (inputArray4: ClArray<int>) (inputArray5: ClArray<int>) (inputArray6: ClArray<int>) (inputArray7: ClArray<int>) (inputArray8: ClArray<int>) (inputArray9: ClArray<int>) (inputArray10: ClArray<int>) -> () @>
    let kernel = <@ fun (ndRange: Range1D) (inputArray1: ClArray<int>) -> () @>

    let OclContextInfo =
        ClDevice.GetAvailableDevices(Platform.Nvidia)
        |> Seq.map (fun x -> printfn $"!!! %A{x}"; RuntimeContext x)
        |> Seq.head

    let OclContext: ClContext = OclContextInfo.ClContext
    let WorkGroupSize = 64

    let Processor =
        let p = OclContextInfo.ClContext.QueueProvider.CreateQueue()
        p
    member val kernelCompiled = None with get, set

    member val DeviceArray1 = Unchecked.defaultof<ClArray<int>> with get, set
    member val DeviceArray2 = Unchecked.defaultof<ClArray<int>> with get, set
    member val DeviceArray3 = Unchecked.defaultof<ClArray<int>> with get, set
    member val DeviceArray4 = Unchecked.defaultof<ClArray<int>> with get, set
    member val DeviceArray5 = Unchecked.defaultof<ClArray<int>> with get, set
    member val DeviceArray6 = Unchecked.defaultof<ClArray<int>> with get, set
    member val DeviceArray7 = Unchecked.defaultof<ClArray<int>> with get, set
    member val DeviceArray8 = Unchecked.defaultof<ClArray<int>> with get, set
    member val DeviceArray9 = Unchecked.defaultof<ClArray<int>> with get, set
    member val DeviceArray10 = Unchecked.defaultof<ClArray<int>> with get, set

    member val HostArray = Unchecked.defaultof<int []> with get, set

    member val ArraySize = 1000000

    member val Iterations = 1000

    member this.ClearInputArrays() =
        this.DeviceArray1.FreeAndWait Processor
        this.DeviceArray2.FreeAndWait Processor
        this.DeviceArray3.FreeAndWait Processor
        this.DeviceArray4.FreeAndWait Processor
        this.DeviceArray5.FreeAndWait Processor
        this.DeviceArray6.FreeAndWait Processor
        this.DeviceArray7.FreeAndWait Processor
        this.DeviceArray8.FreeAndWait Processor
        this.DeviceArray9.FreeAndWait Processor
        this.DeviceArray10.FreeAndWait Processor

    member this.CreateArray()  =
        this.HostArray <- Array.create this.ArraySize 1

    member this.LoadArraysToGPU() =
        this.DeviceArray1 <- OclContext.CreateClArrayWithSpecificAllocationMode(DeviceOnly, this.HostArray)
        this.DeviceArray2 <- OclContext.CreateClArrayWithSpecificAllocationMode(DeviceOnly, this.HostArray)
        this.DeviceArray3 <- OclContext.CreateClArrayWithSpecificAllocationMode(DeviceOnly, this.HostArray)
        this.DeviceArray4 <- OclContext.CreateClArrayWithSpecificAllocationMode(DeviceOnly, this.HostArray)
        this.DeviceArray5 <- OclContext.CreateClArrayWithSpecificAllocationMode(DeviceOnly, this.HostArray)
        this.DeviceArray6 <- OclContext.CreateClArrayWithSpecificAllocationMode(DeviceOnly, this.HostArray)
        this.DeviceArray7 <- OclContext.CreateClArrayWithSpecificAllocationMode(DeviceOnly, this.HostArray)
        this.DeviceArray8 <- OclContext.CreateClArrayWithSpecificAllocationMode(DeviceOnly, this.HostArray)
        this.DeviceArray9 <- OclContext.CreateClArrayWithSpecificAllocationMode(DeviceOnly, this.HostArray)
        this.DeviceArray10 <- OclContext.CreateClArrayWithSpecificAllocationMode(DeviceOnly, this.HostArray)

    [<GlobalSetup>]
    member this.GlobalSetup() =
        this.kernelCompiled <- Some (OclContext.Compile kernel)
        this.CreateArray()
        this.LoadArraysToGPU()

    [<IterationSetup>]
    member this.IterationSetup() =
        Processor.Synchronize()

    [<Benchmark>]
    member this.Benchmark() =
        let mutable i = 0
        while i < this.Iterations do
            let ndRange =
                Range1D.CreateValid(1, WorkGroupSize)

            let kernel = this.kernelCompiled.Value.GetKernel()
            //Processor.Post(Msg.MsgSetArguments(fun () -> kernel.KernelFunc ndRange this.DeviceArray1 this.DeviceArray2 this.DeviceArray3 this.DeviceArray4 this.DeviceArray5 this.DeviceArray6 this.DeviceArray7 this.DeviceArray8 this.DeviceArray9 this.DeviceArray10))
            Processor.Post(Msg.MsgSetArguments(fun () -> kernel.KernelFunc ndRange this.DeviceArray1))
            Processor.Post(Msg.CreateRunMsg<_, _>(kernel))
            Processor.Synchronize()
            i <- i + 1

    [<IterationCleanup>]
    member this.IterationCleanup() = ()


    [<GlobalCleanup>]
    member this.GlobalCleanup() =
        this.ClearInputArrays()
        Processor.Stop()
