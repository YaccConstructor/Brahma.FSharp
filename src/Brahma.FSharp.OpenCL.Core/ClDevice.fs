namespace Brahma.FSharp

open System.Text.RegularExpressions

type ClPlatform = OpenCL.Net.Platform
type ClDeviceType = OpenCL.Net.DeviceType
type ClErrorCode = OpenCL.Net.ErrorCode
type Cl = OpenCL.Net.Cl

exception EmptyDevicesException of string

module internal DeviceHelpers =
    let convertToDeviceType (deviceType: DeviceType) =
        match deviceType with
        | DeviceType.CPU -> ClDeviceType.Cpu
        | DeviceType.GPU -> ClDeviceType.Gpu
        | DeviceType.Default -> ClDeviceType.Default

    let convertToPattern (platform: Platform) =
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

        if error.Value <> ClErrorCode.Success then onError else result

    let (|Contains|_|) (substring: string) (str: string) = if str.Contains substring then Some Contains else None

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
            fun e ->
                Cl
                    .GetDeviceInfo(device, OpenCL.Net.DeviceInfo.MaxWorkGroupSize, e)
                    .CastTo<int>()
            |> throwOnError

        member val MaxWorkItemDimensions =
            fun e ->
                Cl
                    .GetDeviceInfo(device, OpenCL.Net.DeviceInfo.MaxWorkItemDimensions, e)
                    .CastTo<int>()
            |> throwOnError

        // TODO change length
        member val MaxWorkItemSizes =
            fun e ->
                Cl
                    .GetDeviceInfo(device, OpenCL.Net.DeviceInfo.MaxWorkItemSizes, e)
                    .CastToArray<int>(3)
            |> throwOnError

        member val LocalMemSize =
            fun e ->
                Cl.GetDeviceInfo(device, OpenCL.Net.DeviceInfo.LocalMemSize, e).CastTo<int>()
                * 1<Byte>
            |> throwOnError

        member val GlobalMemSize =
            fun e ->
                Cl.GetDeviceInfo(device, OpenCL.Net.DeviceInfo.GlobalMemSize, e).CastTo<int64>()
                * 1L<Byte>
            |> throwOnError

        member val DeviceExtensions =
            let toDeviceExtension (s: string) =
                match s.ToLowerInvariant().Trim() with
                | "cl_intel_accelerator" -> CL_INTEL_ACCELERATOR
                | "cl_intel_advanced_motion_estimation" -> CL_INTEL_ADVANCED_MOTION_ESTIMATION
                | "cl_intel_command_queue_families" -> CL_INTEL_COMMAND_QUEUE_FAMILIES
                | "cl_intel_device_attribute_query" -> CL_INTEL_DEVICE_ATTRIBUTE_QUERY
                | "cl_intel_device_side_avc_motion_estimation" -> CL_INTEL_DEVICE_SIDE_AVC_MOTION_ESTIMATION
                | "cl_intel_driver_diagnostics" -> CL_INTEL_DRIVER_DIAGNOSTICS
                | "cl_intel_media_block_io" -> CL_INTEL_MEDIA_BLOCK_IO
                | "cl_intel_mem_force_host_memory" -> CL_INTEL_MEM_FORCE_HOST_MEMORY
                | "cl_intel_motion_estimation" -> CL_INTEL_MOTION_ESTIMATION
                | "cl_intel_packed_yuv" -> CL_INTEL_PACKED_YUV
                | "cl_intel_planar_yuv" -> CL_INTEL_PLANAR_YUV
                | "cl_intel_required_subgroup_size" -> CL_INTEL_REQUIRED_SUBGROUP_SIZE
                | "cl_intel_sharing_format_query" -> CL_INTEL_SHARING_FORMAT_QUERY
                | "cl_intel_spirv_device_side_avc_motion_estimation" -> CL_INTEL_SPIRV_DEVICE_SIDE_AVC_MOTION_ESTIMATION
                | "cl_intel_spirv_media_block_io" -> CL_INTEL_SPIRV_MEDIA_BLOCK_IO
                | "cl_intel_spirv_subgroups" -> CL_INTEL_SPIRV_SUBGROUPS
                | "cl_intel_split_work_group_barrier" -> CL_INTEL_SPLIT_WORK_GROUP_BARRIER
                | "cl_intel_subgroups" -> CL_INTEL_SUBGROUPS
                | "cl_intel_subgroups_char" -> CL_INTEL_SUBGROUPS_CHAR
                | "cl_intel_subgroups_long" -> CL_INTEL_SUBGROUPS_LONG
                | "cl_intel_subgroups_short" -> CL_INTEL_SUBGROUPS_SHORT
                | "cl_intel_unified_shared_memory" -> CL_INTEL_UNIFIED_SHARED_MEMORY
                | "cl_intel_va_api_media_sharing" -> CL_INTEL_VA_API_MEDIA_SHARING
                | "cl_khr_3d_image_writes" -> CL_KHR_3D_IMAGE_WRITES
                | "cl_khr_byte_addressable_store" -> CL_KHR_BYTE_ADDRESSABLE_STORE
                | "cl_khr_create_command_queue" -> CL_KHR_CREATE_COMMAND_QUEUE
                | "cl_khr_depth_images" -> CL_KHR_DEPTH_IMAGES
                | "cl_khr_device_uuid" -> CL_KHR_DEVICE_UUID
                | "cl_khr_fp16" -> CL_KHR_FP16
                | "cl_khr_fp64" -> CL_KHR_FP64
                | "cl_khr_global_int32_base_atomics" -> CL_KHR_GLOBAL_INT32_BASE_ATOMICS
                | "cl_khr_global_int32_extended_atomics" -> CL_KHR_GLOBAL_INT32_EXTENDED_ATOMICS
                | "cl_khr_gl_sharing" -> CL_KHR_GL_SHARING
                | "cl_khr_icd" -> CL_KHR_ICD
                | "cl_khr_il_program" -> CL_KHR_IL_PROGRAM
                | "cl_khr_image2d_from_buffer" -> CL_KHR_IMAGE2D_FROM_BUFFER
                | "cl_khr_int64_base_atomics" -> CL_KHR_INT64_BASE_ATOMICS
                | "cl_khr_int64_extended_atomics" -> CL_KHR_INT64_EXTENDED_ATOMICS
                | "cl_khr_local_int32_base_atomics" -> CL_KHR_LOCAL_INT32_BASE_ATOMICS
                | "cl_khr_local_int32_extended_atomics" -> CL_KHR_LOCAL_INT32_EXTENDED_ATOMICS
                | "cl_khr_mipmap_image" -> CL_KHR_MIPMAP_IMAGE
                | "cl_khr_mipmap_image_writes" -> CL_KHR_MIPMAP_IMAGE_WRITES
                | "cl_khr_pci_bus_info" -> CL_KHR_PCI_BUS_INFO
                | "cl_khr_priority_hints" -> CL_KHR_PRIORITY_HINTS
                | "cl_khr_spir" -> CL_KHR_SPIR
                | "cl_khr_spirv_no_integer_wrap_decoration" -> CL_KHR_SPIRV_NO_INTEGER_WRAP_DECORATION
                | "cl_khr_subgroups" -> CL_KHR_SUBGROUPS
                | "cl_khr_subgroup_ballot" -> CL_KHR_SUBGROUP_BALLOT
                | "cl_khr_subgroup_clustered_reduce" -> CL_KHR_SUBGROUP_CLUSTERED_REDUCE
                | "cl_khr_subgroup_extended_types" -> CL_KHR_SUBGROUP_EXTENDED_TYPES
                | "cl_khr_subgroup_non_uniform_arithmetic" -> CL_KHR_SUBGROUP_NON_UNIFORM_ARITHMETIC
                | "cl_khr_subgroup_non_uniform_vote" -> CL_KHR_SUBGROUP_NON_UNIFORM_VOTE
                | "cl_khr_subgroup_shuffle" -> CL_KHR_SUBGROUP_SHUFFLE
                | "cl_khr_subgroup_shuffle_relative" -> CL_KHR_SUBGROUP_SHUFFLE_RELATIVE
                | "cl_khr_suggested_local_work_size" -> CL_KHR_SUGGESTED_LOCAL_WORK_SIZE
                | "cl_khr_throttle_hints" -> CL_KHR_THROTTLE_HINTS
                | "cl_nv_compiler_options" -> CL_NV_COMPILER_OPTIONS
                | "cl_nv_copy_opts" -> CL_NV_COPY_OPTS
                | "cl_nv_create_buffer" -> CL_NV_CREATE_BUFFER
                | "cl_nv_device_attribute_query" -> CL_NV_DEVICE_ATTRIBUTE_QUERY
                | "cl_nv_pragma_unroll" -> CL_NV_PRAGMA_UNROLL
                | x -> OTHER x

            fun e ->
                Cl
                    .GetDeviceInfo(device, OpenCL.Net.DeviceInfo.Extensions, e)
                    .ToString()
                    .Trim()
                    .Split ' '
                |> Array.map toDeviceExtension
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

    /// Returns a list of extensions. OTHER contains a string value of extension name if it is not represented as a separated case.
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

        let platformNameRegex =
            Regex(wildcardToRegex <| DeviceHelpers.convertToPattern platform, RegexOptions.IgnoreCase)

        let error = ref Unchecked.defaultof<ClErrorCode>

        Cl.GetPlatformIDs error
        |> Seq.choose (fun platform ->
            let platformName =
                Cl.GetPlatformInfo(platform, OpenCL.Net.PlatformInfo.Name, error).ToString()

            if platformNameRegex.Match(platformName).Success then
                Some
                <| Cl.GetDeviceIDs(platform, DeviceHelpers.convertToDeviceType deviceType, error)
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
        with :? System.ArgumentException as ex ->
            raise
            <| EmptyDevicesException $"No %A{deviceType} devices on platform %A{platform} were found"
