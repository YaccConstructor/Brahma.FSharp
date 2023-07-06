namespace Brahma.FSharp.OpenCL.Shared

open OpenCL.Net
open System

type IClMem =
    abstract member Size: IntPtr
    abstract member Data: obj

type IBuffer<'a> =
    inherit IClMem
    inherit IDisposable

    abstract Memory: IMem
    abstract Length: int
    abstract ElementSize: int
    abstract Free: unit -> unit
    abstract Item: int -> 'a with get, set
