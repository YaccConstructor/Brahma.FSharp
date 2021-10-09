namespace Brahma.FSharp.OpenCL

open System
open Brahma.FSharp.OpenCL.Translator

type ClArray<'a when 'a : struct>(buffer: Buffer<'a>) =
    member internal this.Buffer = buffer

    member this.Length = buffer.Length

    member this.Item
        with get (idx: int) : 'a = FailIfOutsideKernel()
        and set (idx: int) (value: 'a) = FailIfOutsideKernel()

    member this.Dispose() = (this :> IDisposable).Dispose()

    interface IDisposable with
        member this.Dispose() = buffer.Dispose()

    interface IClMem with
        member this.Size = (buffer :> IClMem).Size
        member this.Data = (buffer :> IClMem).Data

    override this.ToString() =
        sprintf "%O, %A" (buffer :> IClMem).Data (buffer :> IClMem).Size

type ClCell<'a when 'a : struct>(buffer: Buffer<'a>) =
    member internal this.Buffer = buffer

    // static member inline (!) (cell: ClCell<'a>) : 'a = failIfOutsideKernel ()

    member this.Dispose() = (this :> IDisposable).Dispose()

    interface IDisposable with
        member this.Dispose() = buffer.Dispose()

    interface IClMem with
        member this.Size = (buffer :> IClMem).Size
        member this.Data = (buffer :> IClMem).Data

// fsharplint:disable-next-line
type clarray<'a when 'a : struct> = ClArray<'a>

// fsharplint:disable-next-line
type clcell<'a when 'a : struct> = ClCell<'a>

module ClArray =
    let toDevice (array: 'a[]) = opencl {
        let! context = ClTask.ask

        let buffer = new Buffer<'a>(context.Provider, Data array, { ClMemFlags.Default  with AllocationMode = AllocationMode.UseHostPtr })
        // context.CommandQueue.Post <| Msg.CreateToHostMsg(buffer, array)
        return new ClArray<'a>(buffer)
    }

    let toHost (clArray: ClArray<'a>) = opencl {
        let! context = ClTask.ask

        let array = Array.zeroCreate<'a> clArray.Length
        context.Provider.CommandQueue.Post <| Msg.CreateToHostMsg(clArray.Buffer, array)

        return array
    }

    // TODO impl it
    let copy (clArray: ClArray<'a>) = opencl { return clArray }

    let alloc<'a when 'a : struct> (size: int) = opencl {
        let! context = ClTask.ask

        let buffer = new Buffer<'a>(context.Provider, Size size)
        return new ClArray<'a>(buffer)
    }

module ClCell =
    let toDevice (value: 'a) = opencl {
        let! context = ClTask.ask

        let buffer = new Buffer<'a>(context.Provider, Size 1)
        context.Provider.CommandQueue.Post <| Msg.CreateToHostMsg(buffer, [| value |])
        return new ClCell<'a>(buffer)
    }

    let toHost (clCell: ClCell<'a>) = opencl {
        let! context = ClTask.ask

        let array = Array.zeroCreate<'a> 1
        context.Provider.CommandQueue.Post <| Msg.CreateToHostMsg(clCell.Buffer, array)
        return array.[0]
    }

    let copy (clCell: ClCell<'a>) = opencl { return clCell }

    let alloc<'a when 'a : struct> () = opencl {
        let! context = ClTask.ask

        let buffer = new Buffer<'a>(context.Provider, Size 1)
        return new ClCell<'a>(buffer)
    }
