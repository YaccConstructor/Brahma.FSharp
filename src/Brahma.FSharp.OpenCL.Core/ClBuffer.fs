namespace Brahma.FSharp

open OpenCL.Net
open System
open System.Runtime.InteropServices
open Brahma.FSharp.OpenCL.Shared

//memory flags: https://www.khronos.org/registry/OpenCL/sdk/1.2/docs/man/xhtml/clCreateBuffer.html

exception InvalidMemFlagsException of string

[<RequireQualifiedAccess>]
type HostAccessMode =
    | ReadWrite
    | ReadOnly
    | WriteOnly
    | NotAccessible

[<RequireQualifiedAccess>]
type DeviceAccessMode =
    | ReadWrite
    | ReadOnly
    | WriteOnly

[<RequireQualifiedAccess>]
type AllocationMode =
    | UseHostPtr
    | AllocHostPtr
    | CopyHostPtr
    | AllocAndCopyHostPtr
    | Default

/// Represents flags to specify allocation and usage information of OpenCL buffer.
type ClMemFlags =
    { HostAccessMode: HostAccessMode
      DeviceAccessMode: DeviceAccessMode
      AllocationMode: AllocationMode }

    /// Represents default flags in case of allocation with copying data.
    static member DefaultIfData =
        { HostAccessMode = HostAccessMode.ReadWrite
          DeviceAccessMode = DeviceAccessMode.ReadWrite
          AllocationMode = AllocationMode.AllocAndCopyHostPtr }

    /// Represents default flags in case of allocation without copying data.
    static member DefaultIfNoData =
        { HostAccessMode = HostAccessMode.ReadWrite
          DeviceAccessMode = DeviceAccessMode.ReadWrite
          AllocationMode = AllocationMode.AllocHostPtr }

type BufferInitParam<'a> =
    | Data of 'a[]
    | Size of int

/// Represents an abstraction over OpenCL memory buffer.
type ClBuffer<'a>(clContext: ClContext, initParam: BufferInitParam<'a>, ?memFlags: ClMemFlags) =

    let memFlags =
        match initParam with
        | Data _ -> ClMemFlags.DefaultIfData
        | Size _ -> ClMemFlags.DefaultIfNoData
        |> defaultArg memFlags

    let marshaler = clContext.Translator.Marshaller

    let intPtrSize = IntPtr(Marshal.SizeOf typedefof<IntPtr>)

    let clMemoryFlags =
        let mutable flags = MemFlags.None

        match memFlags.HostAccessMode with
        | HostAccessMode.ReadWrite -> ()
        | HostAccessMode.ReadOnly -> flags <- flags ||| MemFlags.HostReadOnly
        | HostAccessMode.WriteOnly -> flags <- flags ||| MemFlags.HostWriteOnly
        | HostAccessMode.NotAccessible -> flags <- flags ||| MemFlags.HostNoAccess

        match memFlags.DeviceAccessMode with
        | DeviceAccessMode.ReadWrite -> flags <- flags ||| MemFlags.ReadWrite // default
        | DeviceAccessMode.ReadOnly -> flags <- flags ||| MemFlags.ReadOnly
        | DeviceAccessMode.WriteOnly -> flags <- flags ||| MemFlags.WriteOnly

        let ifDataFlags =
            [ AllocationMode.UseHostPtr
              AllocationMode.CopyHostPtr
              AllocationMode.AllocAndCopyHostPtr ]

        match initParam with
        | Size _ when ifDataFlags |> List.contains memFlags.AllocationMode ->
            raise
            <| InvalidMemFlagsException $"One of following flags should be setted {ifDataFlags}"
        | Data _ when ifDataFlags |> List.contains memFlags.AllocationMode |> not ->
            raise
            <| InvalidMemFlagsException $"Neither of following flags should be setted {ifDataFlags}"
        | _ -> ()

        match memFlags.AllocationMode with
        | AllocationMode.UseHostPtr -> flags <- flags ||| MemFlags.UseHostPtr
        | AllocationMode.AllocHostPtr -> flags <- flags ||| MemFlags.AllocHostPtr
        | AllocationMode.CopyHostPtr -> flags <- flags ||| MemFlags.CopyHostPtr
        | AllocationMode.AllocAndCopyHostPtr -> flags <- flags ||| MemFlags.AllocHostPtr ||| MemFlags.CopyHostPtr
        | AllocationMode.Default -> ()

        flags

    let mutable pinnedMemory: GCHandle option = None

    let buffer =
        let error = ref Unchecked.defaultof<ErrorCode>

        let buf =
            if marshaler.IsBlittable typeof<'a> then
                let elementSize = Marshal.SizeOf Unchecked.defaultof<'a>

                let (size, data) =
                    match initParam with
                    | Data array ->
                        pinnedMemory <- Some <| GCHandle.Alloc(array, GCHandleType.Pinned)
                        IntPtr(array.Length * elementSize), array
                    | Size size -> IntPtr(size * elementSize), null

                Cl.CreateBuffer(clContext.Context, clMemoryFlags, size, data, error)

            else
                match initParam with
                | Data array ->
                    let (size, data) = marshaler.WriteToUnmanaged(array)

                    let buffer =
                        Cl.CreateBuffer(clContext.Context, clMemoryFlags, IntPtr size, data, error)

                    Marshal.FreeHGlobal(data)
                    buffer

                | Size size ->
                    let size = IntPtr(size * marshaler.GetTypePacking(typeof<'a>).Size)
                    Cl.CreateBuffer(clContext.Context, clMemoryFlags, size, null, error)

        if error.Value <> ErrorCode.Success then
            raise <| Cl.Exception error.Value

        buf

    member this.ClContext = clContext

    interface IBuffer<'a> with
        member this.Memory = buffer

        member this.Length =
            match initParam with
            | Data array -> array.Length
            | Size size -> size

        member this.ElementSize = marshaler.GetTypePacking(typeof<'a>).Size

        member this.Free() =
            match pinnedMemory with
            | Some x -> x.Free()
            | None -> ()

            buffer.Dispose()

        member this.Item
            with get (idx: int): 'a = FailIfOutsideKernel()
            and set (idx: int) (value: 'a) = FailIfOutsideKernel()

    interface IDisposable with
        member this.Dispose() = (this :> IBuffer<'a>).Free()

    interface IClMem with
        member this.Size = intPtrSize
        member this.Data = box buffer

    member this.Dispose() = (this :> IDisposable).Dispose()
    member this.Length = (this :> IBuffer<'a>).Length
