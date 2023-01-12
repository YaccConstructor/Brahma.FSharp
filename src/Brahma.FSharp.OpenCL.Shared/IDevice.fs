namespace Brahma.FSharp

[<RequireQualifiedAccess>]
type Platform =
    | Intel
    | Amd
    | Nvidia
    | Any
    | Custom of pattern: string

[<RequireQualifiedAccess>]
type DeviceType =
    | CPU
    | GPU
    | Default

type [<Measure>] Byte

type IDevice =
    abstract Name: string
    abstract Platform: Platform
    abstract DeviceType: DeviceType

    abstract MaxWorkGroupSize: int
    abstract MaxWorkItemDimensions: int
    abstract MaxWorkItemSizes: int[]

    abstract LocalMemSize: int<Byte>
    abstract GlobalMemSize: int64<Byte>

    abstract DeviceExtensions: string
