namespace Brahma.FSharp

open Brahma.FSharp.OpenCL.Shared
open System

/// Represents an abstraction over value in OpenCL device memory.
type ClCell<'a> internal (buffer: ClBuffer<'a>) =
    member internal this.Buffer = buffer

    /// Gets internal value.
    member this.Value
        with get (): 'a = FailIfOutsideKernel()
        and set (value: 'a) = FailIfOutsideKernel()

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
            with get (idx: int): 'a = FailIfOutsideKernel()
            and set (idx: int) (value: 'a) = FailIfOutsideKernel()

    member this.Dispose() = (this :> IDisposable).Dispose()

// fsharplint:disable-next-line
type clcell<'a> = ClCell<'a>

module ClCell =
    /// Transfers specified value to device with specified memory flags.
    let toDeviceWithFlags (value: 'a) (memFlags: ClMemFlags) =
        opencl {
            let! context = ClTask.ask

            let buffer = new ClBuffer<'a>(context.ClContext, Data [| value |], memFlags)
            return new ClCell<'a>(buffer)
        }

    /// Transfers specified value to device with default memory flags.
    let toDevice (value: 'a) = toDeviceWithFlags value ClMemFlags.DefaultIfData

    /// Allocate default value on device with specified memory flags.
    let allocWithFlags<'a> (memFlags: ClMemFlags) =
        opencl {
            let! context = ClTask.ask

            let buffer = new ClBuffer<'a>(context.ClContext, Size 1, memFlags)
            return new ClCell<'a>(buffer)
        }

    /// Allocate empty array on device with default memory flags.
    let alloc<'a> () = allocWithFlags<'a> ClMemFlags.DefaultIfNoData

    /// Transfers specified value from device to host.
    let toHost (clCell: ClCell<'a>) =
        opencl {
            let! context = ClTask.ask

            let array = Array.zeroCreate<'a> 1

            return context.CommandQueue.PostAndReply(fun ch -> Msg.CreateToHostMsg(clCell.Buffer, array, ch)).[0]
        }

    // TODO impl it
    let copy (clCell: ClCell<'a>) = opencl { failwith "Not implemented yet" }
