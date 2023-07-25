namespace Brahma.FSharp

open Brahma.FSharp.OpenCL.Shared
open System

/// Represents an abstraction over array in OpenCL device memory.
type ClArray<'a> internal (buffer: ClBuffer<'a>) =
    member internal this.Buffer = buffer

    /// Gets array length.
    member this.Length = buffer.Length

    member this.Item
        with get (idx: int) : 'a = FailIfOutsideKernel()
        and set (idx: int) (value: 'a) = FailIfOutsideKernel()

    interface IDisposable with
        member this.Dispose() = buffer.Dispose()

    interface IClMem with
        member this.Size = (buffer :> IClMem).Size
        member this.Data = (buffer :> IClMem).Data

    interface IBuffer<'a> with
        member this.Memory = (buffer :> IBuffer<_>).Memory
        member this.Length = (buffer :> IBuffer<_>).Length
        member this.ElementSize = (buffer :> IBuffer<_>).ElementSize
        member this.Free() = (buffer :> IBuffer<_>).Free()

        member this.Item
            with get (idx: int) : 'a = FailIfOutsideKernel()
            and set (idx: int) (value: 'a) = FailIfOutsideKernel()

    member this.Dispose() = (this :> IDisposable).Dispose()

    override this.ToString() =
        $"{(buffer :> IClMem).Data}, %A{(buffer :> IClMem).Size}"

// fsharplint:disable-next-line
type clarray<'a> = ClArray<'a>

module ClArray =
    /// Transfers specified array to device with specified memory flags.
    let toDeviceWithFlags (array: 'a[]) (memFlags: ClMemFlags) = opencl {
        let! context = ClTask.ask

        let buffer = new ClBuffer<'a>(context.ClContext, Data array, memFlags)
        return new ClArray<'a>(buffer)
    }

    // or allocate with null ptr and write
    // TODO if array.Length = 0 ...
    /// Transfers specified array to device with default memory flags.
    let toDevice (array: 'a[]) = toDeviceWithFlags array ClMemFlags.DefaultIfData

    /// Allocate empty array on device with specified memory flags.
    let allocWithFlags<'a> (size: int) (memFlags: ClMemFlags) = opencl {
        let! context = ClTask.ask

        let buffer = new ClBuffer<'a>(context.ClContext, Size size, memFlags)
        return new ClArray<'a>(buffer)
    }

    /// Allocate empty array on device with default memory flags.
    let alloc<'a> (size: int) = allocWithFlags<'a> size ClMemFlags.DefaultIfNoData

    /// Transfers specified array from device to host.
    let toHost (clArray: ClArray<'a>) = opencl {
        let! context = ClTask.ask

        let array = Array.zeroCreate<'a> clArray.Length
        return context.CommandQueue.PostAndReply(fun ch -> Msg.CreateToHostMsg(clArray.Buffer, array, ch))
    }

    // TODO impl it using clEnqueCopy
    let copy (clArray: ClArray<'a>) = opencl {
        failwith "Not implemented yet"
    }

    // TODO impl it
    let copyTo (destination: ClArray<'a>) (source: ClArray<'a>) = opencl {
        failwith "Not implemented yet"
    }

    let close (clArray: ClArray<'a>) = opencl {
        let! ctx = ClTask.ask
        ctx.CommandQueue.Post <| Msg.CreateFreeMsg(clArray)
    }
