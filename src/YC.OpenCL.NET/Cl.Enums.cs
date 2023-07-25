#region License and Copyright Notice
// Copyright (c) 2010 Ananth B.
// All rights reserved.
// 
// The contents of this file are made available under the terms of the
// Eclipse Public License v1.0 (the "License") which accompanies this
// distribution, and is available at the following URL:
// http://www.opensource.org/licenses/eclipse-1.0.php
// 
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
// the specific language governing rights and limitations under the License.
// 
// By using this software in any fashion, you are agreeing to be bound by the
// terms of the License.
#endregion

using System;

namespace OpenCL.Net
{
    public enum ProfilingInfo: int // cl_int
    {
        Queued = 0x1280,
        Submit = 0x1281,
        Start = 0x1282,
        End = 0x1283,
    };

    public enum SamplerInfo : uint // cl_uint
    {
        ReferenceCount = 0x1150,
        Context = 0x1151,
        NormalizedCoords = 0x1152,
        AddressingMode = 0x1153,
        FilterMode = 0x1154,
    };

    public enum FilterMode : uint // cl_uint
    {
        Nearest = 0x1140,
        Linear = 0x1141,
    };

    public enum AddressingMode : uint // cl_uint
    {
        None = 0x1130,
        ClampToEdge = 0x1131,
        Clamp = 0x1132,
        Repeat = 0x1133,
    };

    public enum ExecutionStatus: int // cl_int
    {
        Complete = 0x0,
        Running = 0x1,
        Submitted = 0x2,
        Queued = 0x3,
    };

    public enum EventInfo: int // cl_int
    {
        CommandQueue = 0x11D0,
        CommandType = 0x11D1,
        ReferenceCount = 0x11D2,
        CommandExecutionStatus = 0x11D3,
    };

    [Flags]
    public enum MapFlags: int // cl_int
    {
        Read = (1 << 0),
        Write = (1 << 1),
    };

    public enum KernelWorkGroupInfo : int // cl_int
    {
        WorkGroupSize = 0x11B0,
        CompileWorkGroupSize = 0x11B1,
        LocalMemSize = 0x11B2
    };

    public enum KernelInfo : int // cl_int
    {
        FunctionName = 0x1190,
        NumArgs = 0x1191,
        ReferenceCount = 0x1192,
        Context = 0x1193,
        Program = 0x1194
    }

    public enum CommandQueueInfo : int // cl_int
    {
        Context = 0x1090,
        Device = 0x1091,
        ReferenceCount = 0x1092,
        Properties = 0x1093
    }

    [Flags]
    public enum CommandQueueProperties : ulong // cl_ulong
    { 
        None = 0,
        OutOfOrderExecModeEnable = (1 << 0),
        ProfilingEnable = (1 << 1)
    }

    public enum BuildStatus : int // cl_int
    {
        Success = 0,
        None = -1,
        Error = -2,
        InProgress = -3
    };

    public enum ProgramBuildInfo : uint // cl_uint
    {
        Status = 0x1181,
        Options = 0x1182,
        Log = 0x1183,
    };

    public enum ProgramInfo : uint // cl_uint
    {
        ReferenceCount = 0x1160,
        Context = 0x1161,
        NumDevices = 0x1162,
        Devices = 0x1163,
        Source = 0x1164,
        BinarySizes = 0x1165,
        Binaries = 0x1166,
    };

    public enum ImageInfo : uint // cl_uint
    {
        Format = 0x1110,
        ElementSize = 0x1111,
        RowPitch = 0x1112,
        SlicePitch = 0x1113,
        Width = 0x1114,
        Height = 0x1115,
        Depth = 0x1116,
    };

    public enum MemInfo : uint // cl_uint
    {
        Type = 0x1100,
        Flags = 0x1101,
        Size = 0x1102,
        HostPtr = 0x1103,
        MapCount = 0x1104,
        ReferenceCount = 0x1105,
        Context = 0x1106,
    };

    public enum MemObjectType : uint // cl_uint
    {
        Buffer = 0x10F0,
        Image2D = 0x10F1,
        Image3D = 0x10F2,
    };

    [Flags]
    public enum MemFlags : ulong // cl_ulong
    {
        None = 0,
        ReadWrite = (1 << 0),
        WriteOnly = (1 << 1),
        ReadOnly = (1 << 2),
        UseHostPtr = (1 << 3),
        AllocHostPtr = (1 << 4),
        CopyHostPtr = (1 << 5),
        HostWriteOnly = (1 << 6),
        HostReadOnly = (1 << 7),
        HostNoAccess = (1 << 8)
    };

    public enum ContextInfo : uint // cl_uint
    {
        ReferenceCount = 0x1080,
        Devices = 0x1081,
        Properties = 0x1082,
    };

    public enum ChannelType : uint // cl_uint
    {
        Snorm_Int8 = 0x10D0,
        Snorm_Int16 = 0x10D1,
        Unorm_Int8 = 0x10D2,
        Unorm_Int16 = 0x10D3,
        Unorm_Short565 = 0x10D4,
        Unorm_Short555 = 0x10D5,
        Unorm_Int101010 = 0x10D6,
        Signed_Int8 = 0x10D7,
        Signed_Int16 = 0x10D8,
        Signed_Int32 = 0x10D9,
        Unsigned_Int8 = 0x10DA,
        Unsigned_Int16 = 0x10DB,
        Unsigned_Int32 = 0x10DC,
        HalfFloat = 0x10DD,
        Float = 0x10DE,
    };

    public enum ChannelOrder : uint // cl_uint
    {
        R = 0x10B0,
        A = 0x10B1,
        RG = 0x10B2,
        RA = 0x10B3,
        RGB = 0x10B4,
        RGBA = 0x10B5,
        BGRA = 0x10B6,
        ARGB = 0x10B7,
        Intensity = 0x10B8,
        Luminance = 0x10B9,
    };

    public enum ContextProperties : uint // cl_uint
    {
        Platform = 0x1084,
    };

    public enum DeviceInfo : uint // cl_uint
    {
        Type = 0x1000,
        VendorId = 0x1001,
        MaxComputeUnits = 0x1002,
        MaxWorkItemDimensions = 0x1003,
        MaxWorkGroupSize = 0x1004,
        MaxWorkItemSizes = 0x1005,
        PreferredVectorWidthChar = 0x1006,
        PreferredVectorWidthShort = 0x1007,
        PreferredVectorWidthInt = 0x1008,
        PreferredVectorWidthLong = 0x1009,
        PreferredVectorWidthFloat = 0x100A,
        PreferredVectorWidthDouble = 0x100B,
        MaxClockFrequency = 0x100C,
        AddressBits = 0x100D,
        MaxReadImageArgs = 0x100E,
        MaxWriteImageArgs = 0x100F,
        MaxMemAllocSize = 0x1010,
        Image2DMaxWidth = 0x1011,
        Image2DMaxHeight = 0x1012,
        Image3DMaxWidth = 0x1013,
        Image3DMaxHeight = 0x1014,
        Image3DMaxDepth = 0x1015,
        ImageSupport = 0x1016,
        MaxParameterSize = 0x1017,
        MaxSamplers = 0x1018,
        MemBaseAddrAlign = 0x1019,
        MinDataTypeAlignSize = 0x101A,
        SingleFpConfig = 0x101B,
        GlobalMemCacheType = 0x101C,
        GlobalMemCachelineSize = 0x101D,
        GlobalMemCacheSize = 0x101E,
        GlobalMemSize = 0x101F,
        MaxConstantBufferSize = 0x1020,
        MaxConstantArgs = 0x1021,
        LocalMemType = 0x1022,
        LocalMemSize = 0x1023,
        ErrorCorrectionSupport = 0x1024,
        ProfilingTimerResolution = 0x1025,
        EndianLittle = 0x1026,
        Available = 0x1027,
        CompilerAvailable = 0x1028,
        ExecutionCapabilities = 0x1029,
        QueueProperties = 0x102A,
        Name = 0x102B,
        Vendor = 0x102C,
        DriverVersion = 0x102D,
        Profile = 0x102E,
        Version = 0x102F,
        Extensions = 0x1030,
        Platform = 0x1031,
    };

    [Flags]
    public enum DeviceType : ulong // cl_ulong
    {
        Default = (1 << 0),
        Cpu = (1 << 1),
        Gpu = (1 << 2),
        Accelerator = (1 << 3),
        All = 0xFFFFFFFF,
    };

    public enum PlatformInfo : uint // cl_uint
    {
        Profile = 0x0900,
        Version = 0x0901,
        Name = 0x0902,
        Vendor = 0x0903,
        Extensions = 0x0904,
    };

    public enum ErrorCode : int // cl_int
    {
        Unknown = 1, // http://forums.nvidia.com/index.php?showtopic=153515

        Success = 0,
        DeviceNotFound = -1,
        DeviceNotAvailable = -2,
        CompilerNotAvailable = -3,
        MemObjectAllocationFailure = -4,
        OutOfResources = -5,
        OutOfHostMemory = -6,
        ProfilingInfoNotAvailable = -7,
        MemCopyOverlap = -8,
        ImageFormatMismatch = -9,
        ImageFormatNotSupported = -10,
        BuildProgramFailure = -11,
        MapFailure = -12,

        InvalidValue = -30,
        InvalidDeviceType = -31,
        InvalidPlatform = -32,
        InvalidDevice = -33,
        InvalidContext = -34,
        InvalidQueueProperties = -35,
        InvalidCommandQueue = -36,
        InvalidHostPtr = -37,
        InvalidMemObject = -38,
        InvalidImageFormatDescriptor = -39,
        InvalidImageSize = -40,
        InvalidSampler = -41,
        InvalidBinary = -42,
        InvalidBuildOptions = -43,
        InvalidProgram = -44,
        InvalidProgramExecutable = -45,
        InvalidKernelName = -46,
        InvalidKernelDefinition = -47,
        InvalidKernel = -48,
        InvalidArgIndex = -49,
        InvalidArgValue = -50,
        InvalidArgSize = -51,
        InvalidKernelArgs = -52,
        InvalidWorkDimension = -53,
        InvalidWorkGroupSize = -54,
        InvalidWorkItemSize = -55,
        InvalidGlobalOffset = -56,
        InvalidEventWaitList = -57,
        InvalidEvent = -58,
        InvalidOperation = -59,
        InvalidGlObject = -60,
        InvalidBufferSize = -61,
        InvalidMipLevel = -62,
    };

    public enum Bool : uint // cl_uint
    {
        False = 0,
        True = 1
    };
}
