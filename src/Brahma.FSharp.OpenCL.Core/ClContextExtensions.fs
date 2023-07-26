namespace Brahma.FSharp

open FSharp.Quotations

[<AutoOpen>]
module ClContextExtensions =
    type ClContext with

        /// Compiles raw kernel to OpenCL program.
        member this.Compile(srcLambda: Expr<'TRange -> 'a>) = ClProgram(this, srcLambda)

        /// Creates OpenCL array based on specified data with specified memory flags.
        member this.CreateClArray
            (
                data: 'a[],
                ?hostAccessMode: HostAccessMode,
                ?deviceAccessMode: DeviceAccessMode,
                ?allocationMode: AllocationMode
            ) =

            let flags =
                {
                    HostAccessMode = defaultArg hostAccessMode ClMemFlags.DefaultIfData.HostAccessMode
                    DeviceAccessMode = defaultArg deviceAccessMode ClMemFlags.DefaultIfData.DeviceAccessMode
                    AllocationMode = defaultArg allocationMode ClMemFlags.DefaultIfData.AllocationMode
                }

            let buffer = new ClBuffer<'a>(this, Data data, flags)
            new ClArray<_>(buffer)

        /// Creates OpenCL empty array based on specified array size with specified memory flags.
        member this.CreateClArray
            (
                size: int,
                ?hostAccessMode: HostAccessMode,
                ?deviceAccessMode: DeviceAccessMode,
                ?allocationMode: AllocationMode
            ) =

            let flags =
                {
                    HostAccessMode = defaultArg hostAccessMode ClMemFlags.DefaultIfNoData.HostAccessMode
                    DeviceAccessMode = defaultArg deviceAccessMode ClMemFlags.DefaultIfNoData.DeviceAccessMode
                    AllocationMode = defaultArg allocationMode ClMemFlags.DefaultIfNoData.AllocationMode
                }

            let buffer = new ClBuffer<'a>(this, Size size, flags)
            new ClArray<_>(buffer)

        /// Creates OpenCL value based on specified data with specified memory flags.
        member this.CreateClCell
            (
                data: 'a,
                ?hostAccessMode: HostAccessMode,
                ?deviceAccessMode: DeviceAccessMode,
                ?allocationMode: AllocationMode
            ) =

            let flags =
                {
                    HostAccessMode = defaultArg hostAccessMode ClMemFlags.DefaultIfData.HostAccessMode
                    DeviceAccessMode = defaultArg deviceAccessMode ClMemFlags.DefaultIfData.DeviceAccessMode
                    AllocationMode = defaultArg allocationMode ClMemFlags.DefaultIfData.AllocationMode
                }

            let buffer = new ClBuffer<'a>(this, Data [| data |], flags)
            new ClCell<_>(buffer)

        /// Creates OpenCL default value with specified memory flags.
        member this.CreateClCell
            (
                ?hostAccessMode: HostAccessMode,
                ?deviceAccessMode: DeviceAccessMode,
                ?allocationMode: AllocationMode
            ) =

            let flags =
                {
                    HostAccessMode = defaultArg hostAccessMode ClMemFlags.DefaultIfNoData.HostAccessMode
                    DeviceAccessMode = defaultArg deviceAccessMode ClMemFlags.DefaultIfNoData.DeviceAccessMode
                    AllocationMode = defaultArg allocationMode ClMemFlags.DefaultIfNoData.AllocationMode
                }

            let buffer = new ClBuffer<'a>(this, Size 1, flags)
            new ClCell<_>(buffer)
