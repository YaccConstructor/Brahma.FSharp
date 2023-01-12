namespace Brahma.FSharp

open System.Text.RegularExpressions

type ClPlatform = OpenCL.Net.Platform
type ClDeviceType = OpenCL.Net.DeviceType
type ClErrorCode = OpenCL.Net.ErrorCode
type Cl = OpenCL.Net.Cl

exception EmptyDevicesException of string

module internal DeviceHelpers =
    let convertToDeviceType(deviceType: DeviceType) =
        match deviceType with
        | DeviceType.CPU -> ClDeviceType.Cpu
        | DeviceType.GPU -> ClDeviceType.Gpu
        | DeviceType.Default -> ClDeviceType.Default

    let convertToPattern(platform: Platform) =
        match platform with
        | Platform.Intel -> "Intel*"
        | Platform.Amd -> "AMD*"
        | Platform.Nvidia -> "NVIDIA*"
        | Platform.Any -> "*"
        | Platform.Custom pattern -> pattern

/// Represents an abstraction over single OpenCL device.
type ClDevice(device: OpenCL.Net.Device) =
    let throwOnError f =
        let error = ref Unchecked.defaultof<ClErrorCode>
        let result = f error
        if error.Value <> ClErrorCode.Success then
            failwithf $"Program creation failed: %A{error}"
        else
            result

    let defaultOnError onError f =
        let error = ref Unchecked.defaultof<ClErrorCode>
        let result = f error
        if error.Value <> ClErrorCode.Success then
            onError
        else
            result

    let (|Contains|_|) (substring: string) (str: string) =
        if str.Contains substring then Some Contains
        else None

    /// Gets internal representation of device specific to OpenCL.Net.
    member this.Device = device

    interface IDevice with
        member val Name =
            fun e -> Cl.GetDeviceInfo(device, OpenCL.Net.DeviceInfo.Name, e).ToString()
            |> defaultOnError ""

        member val Platform =
            fun e ->
                match Cl.GetDeviceInfo(device, OpenCL.Net.DeviceInfo.Vendor, e).ToString() with
                | Contains "NVIDIA" -> Platform.Nvidia
                | Contains "Intel" -> Platform.Intel
                | Contains "AMD" -> Platform.Amd
                | _ -> Platform.Any
            |> defaultOnError Platform.Any

        member val DeviceType =
            fun e ->
                match Cl.GetDeviceInfo(device, OpenCL.Net.DeviceInfo.Type, e).CastTo<ClDeviceType>() with
                | ClDeviceType.Cpu -> DeviceType.CPU
                | ClDeviceType.Gpu -> DeviceType.GPU
                | _ -> DeviceType.Default
            |> defaultOnError DeviceType.Default

        member val MaxWorkGroupSize =
            fun e -> Cl.GetDeviceInfo(device, OpenCL.Net.DeviceInfo.MaxWorkGroupSize, e).CastTo<int>()
            |> throwOnError

        member val MaxWorkItemDimensions =
            fun e -> Cl.GetDeviceInfo(device, OpenCL.Net.DeviceInfo.MaxWorkItemDimensions, e).CastTo<int>()
            |> throwOnError

        // TODO change length
        member val MaxWorkItemSizes =
            fun e -> Cl.GetDeviceInfo(device, OpenCL.Net.DeviceInfo.MaxWorkItemSizes, e).CastToArray<int>(3)
            |> throwOnError

        member val LocalMemSize =
            fun e -> Cl.GetDeviceInfo(device, OpenCL.Net.DeviceInfo.LocalMemSize, e).CastTo<int>() * 1<Byte>
            |> throwOnError
        member val GlobalMemSize =
            fun e -> Cl.GetDeviceInfo(device, OpenCL.Net.DeviceInfo.GlobalMemSize, e).CastTo<int64>() * 1L<Byte>
            |> throwOnError

        member val DeviceExtensions =
            fun e -> Cl.GetDeviceInfo(device, OpenCL.Net.DeviceInfo.Extensions, e).ToString()
            |> throwOnError

    /// Device name string.
    member this.Name = (this :> IDevice).Name

    /// The platform associated with this device.
    member this.Platform = (this :> IDevice).Platform

    /// The OpenCL device type.
    member this.DeviceType = (this :> IDevice).DeviceType

    /// Maximum number of work-items in a work-group executing a kernel using the data parallel execution model. The minimum value is 1.
    member this.MaxWorkGroupSize = (this :> IDevice).MaxWorkGroupSize

    /// Maximum dimensions that specify the global and local work-item IDs used by the data parallel execution model. The minimum value is 3.
    member this.MaxWorkItemDimensions = (this :> IDevice).MaxWorkItemDimensions

    /// Maximum number of work-items that can be specified in each dimension of the work-group. The minimum value is (1, 1, 1).
    member this.MaxWorkItemSizes = (this :> IDevice).MaxWorkItemSizes

    /// Size of local memory arena in bytes. The minimum value is 16 KB.
    member this.LocalMemSize = (this :> IDevice).LocalMemSize
    /// Size of global device memory in bytes.
    member this.GlobalMemSize = (this :> IDevice).GlobalMemSize

    /// Returns a space separated list of extension names.
    member this.DeviceExtensions = (this :> IDevice).DeviceExtensions

    override this.ToString() =
        $"{(this :> IDevice).Name} | {(this :> IDevice).Platform} | {(this :> IDevice).DeviceType}"

    /// <summary>
    /// Returns list of all available OpenCL devices of specified platform and device type.
    /// </summary>
    static member GetAvailableDevices(?platform: Platform, ?deviceType: DeviceType) =
        let platform = defaultArg platform Platform.Any
        let deviceType = defaultArg deviceType DeviceType.Default

        let wildcardToRegex (pattern: string) =
            "^" + Regex.Escape(pattern).Replace("\\*", ".*").Replace("\\?", ".") + "$"

        let platformNameRegex = Regex(wildcardToRegex <| DeviceHelpers.convertToPattern platform, RegexOptions.IgnoreCase)

        let error = ref Unchecked.defaultof<ClErrorCode>

        Cl.GetPlatformIDs error
        |> Seq.choose
            (fun platform ->
                let platformName = Cl.GetPlatformInfo(platform, OpenCL.Net.PlatformInfo.Name, error).ToString()
                if platformNameRegex.Match(platformName).Success then
                    Some <| Cl.GetDeviceIDs(platform, DeviceHelpers.convertToDeviceType deviceType, error)
                else
                    None
            )
        |> Seq.concat
        |> Seq.map ClDevice

    /// <summary>
    /// Returns first available OpenCL device of specified platform and device type or throw exception if there are no available devices.
    /// </summary>
    /// <exception cref="EmptyDevicesException">There are no available devices of specified platform and device type.</exception>
    static member GetFirstAppropriateDevice(?platform: Platform, ?deviceType: DeviceType) =
        let platform = defaultArg platform Platform.Any
        let deviceType = defaultArg deviceType DeviceType.Default

        try
            Seq.head <| ClDevice.GetAvailableDevices(platform, deviceType)
        with
        | :? System.ArgumentException as ex ->
            raise <| EmptyDevicesException $"No %A{deviceType} devices on platform %A{platform} were found"
