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
using System.Linq;
using System.Text.RegularExpressions;
using System.Collections.Generic;

namespace OpenCL.Net
{
    public static partial class Cl
    {
        #region Platform API

        public static Platform[] GetPlatformIDs(out ErrorCode error)
        {
            uint platformCount;

            error = GetPlatformIDs(0, null, out platformCount);
            if (error != ErrorCode.Success)
                return new Platform[0];

            var platformIds = new Platform[platformCount] ;
            error = GetPlatformIDs(platformCount, platformIds, out platformCount);
            if (error != ErrorCode.Success)
                return new Platform[0];
            
            return platformIds;
        }

        public static InfoBuffer GetPlatformInfo(Platform platform, PlatformInfo paramName, out ErrorCode error)
        {
            return GetInfo(Cl.GetPlatformInfo, platform, paramName, out error);
        }

        #endregion

        #region Device API

        public static Device[] GetDeviceIDs(Platform platform, DeviceType deviceType, out ErrorCode error)
        {
            uint deviceCount;
            error = GetDeviceIDs(platform, deviceType, 0, null, out deviceCount);
            if (error != ErrorCode.Success)
                return new Device[0];

            var deviceIds = new Device[deviceCount];
            error = GetDeviceIDs(platform, deviceType, deviceCount, deviceIds, out deviceCount);
            if (error != ErrorCode.Success)
                return new Device[0];
            
            return deviceIds;
        }

        public static InfoBuffer GetDeviceInfo(Device device, DeviceInfo paramName, out ErrorCode error)
        {
            return GetInfo(GetDeviceInfo, device, paramName, out error);
        }

        #endregion

        #region Context API

        public static InfoBuffer GetContextInfo(Context context, ContextInfo paramName, out ErrorCode error)
        {
            return GetInfo(GetContextInfo, context, paramName, out error);
        }

        private static Regex WildcardToRegex(this string pattern)
        {
            return new Regex("^" + Regex.Escape(pattern).
            Replace("\\*", ".*").
            Replace("\\?", ".") + "$", RegexOptions.IgnoreCase);
        }

        public static Context CreateContext(string platformWildCard, DeviceType deviceType, out ErrorCode error)
        {
            var platformNameRegex = WildcardToRegex(platformWildCard);
            
            Platform? currentPlatform = null;
            foreach (Platform platform in GetPlatformIDs(out error))
                if (platformNameRegex.Match(GetPlatformInfo(platform, PlatformInfo.Name, out error).ToString()).Success)
                {
                    currentPlatform = platform;
                    break;
                }

            if (currentPlatform == null)
            {
                error = ErrorCode.InvalidPlatform;
                return Context.Zero;
            }

            var compatibleDevices = from device in GetDeviceIDs(currentPlatform.Value, deviceType, out error)
                                    select device;
            if (!compatibleDevices.Any())
            {
                error = ErrorCode.InvalidDevice;
                return Context.Zero;
            }
            var devices = compatibleDevices.ToArray();

            var context = CreateContext(null, (uint)devices.Length, devices, null, IntPtr.Zero, out error);
            if (error != ErrorCode.Success)
            {
                error = ErrorCode.InvalidContext;
                return Context.Zero;
            }

            return context;
        }

        #endregion 

        #region Memory Object API

        public static IMem CreateBuffer(Context context, MemFlags flags, int size, out ErrorCode errcodeRet)
        {
            return CreateBuffer(context, flags, (IntPtr)size, null, out errcodeRet);
        }

        public static IMem CreateBuffer(Context context, MemFlags flags, IntPtr size, out ErrorCode errcodeRet)
        {
            return CreateBuffer(context, flags, size, null, out errcodeRet);
        }

        public static IMem<T> CreateBuffer<T>(Context context, MemFlags flags, T[] hostData, out ErrorCode errcodeRet)
            where T : struct
        {
            return new Mem<T>(CreateBuffer(context, flags, (IntPtr)(TypeSize<T>.SizeInt * hostData.Length), hostData, out errcodeRet));
        }

        public static IMem<T> CreateBuffer<T>(Context context, MemFlags flags, int length, out ErrorCode errcodeRet)
            where T : struct
        {
            return new Mem<T>(CreateBuffer(context, flags, (IntPtr)(TypeSize<T>.SizeInt * length), null, out errcodeRet));
        }

        public static InfoBuffer GetMemObjectInfo(IMem mem, MemInfo paramName, out ErrorCode error)
        {
            if (paramName == MemInfo.HostPtr) // Handle special case
            {
                IntPtr size = GetInfo(Cl.GetMemObjectInfo, mem, MemInfo.Size, out error).CastTo<IntPtr>();
                var buffer = new InfoBuffer(size);
                error = GetMemObjectInfo(mem, paramName, size, buffer, out size);
                if (error != ErrorCode.Success)
                    return InfoBuffer.Empty;

                return buffer;
            }

            return GetInfo(GetMemObjectInfo, mem, paramName, out error);
        }

        public static InfoBuffer GetImageInfo(IMem image, ImageInfo paramName, out ErrorCode error)
        {
            return GetInfo(GetImageInfo, image, paramName, out error);
        }

        public static ImageFormat[] GetSupportedImageFormats(Context context, MemFlags flags, MemObjectType imageType, out ErrorCode error)
        {
            uint imageFormatCount;
            error = GetSupportedImageFormats(context, flags, imageType, 0, null, out imageFormatCount);
            if (error != ErrorCode.Success)
                return new ImageFormat[0];

            var imageFormats = new ImageFormat[imageFormatCount];
            error = GetSupportedImageFormats(context, flags, imageType, imageFormatCount, imageFormats, out imageFormatCount);
            if (error != ErrorCode.Success)
                return new ImageFormat[0];
            
            return imageFormats;
        }

        #endregion

        #region Program Object API

        public static InfoBuffer GetProgramInfo(Program program, ProgramInfo paramName, out ErrorCode error)
        {
            return GetInfo(GetProgramInfo, program, paramName, out error);
        }

        public static InfoBuffer GetProgramBuildInfo(Program program, Device device, ProgramBuildInfo paramName, out ErrorCode error)
        {
            return GetInfo(GetProgramBuildInfo, program, device, paramName, out error);
        }

        #endregion

        #region Command Queue API

        public static InfoBuffer GetCommandQueueInfo(CommandQueue commandQueue, CommandQueueInfo paramName, out ErrorCode error)
        {
            return GetInfo(GetCommandQueueInfo, commandQueue, paramName, out error);
        }

        public static ErrorCode EnqueueReadBuffer<T>(CommandQueue commandQueue,
                                                  IMem buffer,
                                                  Bool blockingRead,
                                                  int offset,
                                                  int length,
                                                  T[] data,
                                                  int numEventsInWaitList,
                                                  Event[] eventWaitList,
                                                  out Event e) where T : struct
        {
            var elementSize = (int)TypeSize<T>.Size;
            return EnqueueReadBuffer(commandQueue, buffer, blockingRead, (IntPtr)(offset * elementSize), (IntPtr)(length * elementSize), data, (uint)numEventsInWaitList, eventWaitList, out e);
        }

        public static ErrorCode EnqueueReadBuffer<T>(CommandQueue commandQueue,
                                                  IMem<T> buffer,
                                                  Bool blockingRead,
                                                  int offset,
                                                  int length,
                                                  T[] data,
                                                  int numEventsInWaitList,
                                                  Event[] eventWaitList,
                                                  out Event e) where T: struct
        { 
            var elementSize = (int)TypeSize<T>.Size;
            return EnqueueReadBuffer(commandQueue, buffer, blockingRead, (IntPtr)(offset * elementSize), (IntPtr)(length * elementSize), data, (uint)numEventsInWaitList, eventWaitList, out e);
        }

        public static ErrorCode EnqueueReadBuffer<T>(CommandQueue commandQueue,
                                                  IMem<T> buffer,
                                                  Bool blockingRead,
                                                  T[] data,
                                                  int numEventsInWaitList,
                                                  Event[] eventWaitList,
                                                  out Event e) where T : struct
        {
            return EnqueueReadBuffer<T>(commandQueue, buffer, blockingRead, 0, data.Length, data, numEventsInWaitList, eventWaitList, out e);
        }

        public static ErrorCode EnqueueWriteBuffer<T>(CommandQueue commandQueue,
                                                   IMem<T> buffer,
                                                   Bool blockingWrite,
                                                   int offset,
                                                   int length,
                                                   T[] data,
                                                   int numEventsInWaitList,
                                                   Event[] eventWaitList,
                                                   out Event e) where T: struct
        {
            var elementSize = (int)TypeSize<T>.Size;
            return EnqueueWriteBuffer(commandQueue, buffer, blockingWrite, (IntPtr)(offset * elementSize), (IntPtr)(length * elementSize), data, (uint)numEventsInWaitList, eventWaitList, out e);
        }

        public static ErrorCode EnqueueWriteBuffer<T>(CommandQueue commandQueue,
                                                   IMem<T> buffer,
                                                   Bool blockingWrite,
                                                   T[] data,
                                                   int numEventsInWaitList,
                                                   Event[] eventWaitList,
                                                   out Event e) where T: struct
        {
            return EnqueueWriteBuffer(commandQueue, buffer, blockingWrite, 0, data.Length, data, numEventsInWaitList, eventWaitList, out e);
        }

        #endregion

        #region Kernel Object API

        public static Kernel[] CreateKernelsInProgram(Program program, out ErrorCode error)
        {
            uint numKernelsRet;
            error = CreateKernelsInProgram(program, 0, null, out numKernelsRet);
            if (error != ErrorCode.Success)
                return null;

            var result = new Kernel[numKernelsRet];
            error = CreateKernelsInProgram(program, numKernelsRet, result, out numKernelsRet);
            if (error != ErrorCode.Success)
                return null;

            return result;
        }

        public static InfoBuffer GetKernelInfo(Kernel kernel, KernelInfo paramName, out ErrorCode error)
        {
            return GetInfo(GetKernelInfo, kernel, paramName, out error);
        }

        public static InfoBuffer GetKernelWorkGroupInfo(Kernel kernel, Device device, KernelWorkGroupInfo paramName, out ErrorCode error)
        {
            return GetInfo(GetKernelWorkGroupInfo, kernel, device, paramName, out error);
        }

        public static ErrorCode SetKernelArg<T>(Kernel kernel, uint argIndex, T value)
            where T: struct
        { 
            return SetKernelArg(kernel, argIndex, (IntPtr)TypeSize<T>.Size, value);
        }

        public static ErrorCode SetKernelArg<T>(Kernel kernel, uint argIndex, IMem<T> value)
            where T : struct
        {
            return SetKernelArg(kernel, argIndex, (IntPtr)TypeSize<IntPtr>.Size, value);
        }

        public static ErrorCode SetKernelArg(Kernel kernel, uint argIndex, IMem value)
        {
            return SetKernelArg(kernel, argIndex, (IntPtr)TypeSize<IntPtr>.Size, value);
        }

        public static ErrorCode SetKernelArg<T>(Kernel kernel, uint argIndex, int length)
        {
            var size = TypeSize<T>.SizeInt * length;
            return SetKernelArg(kernel, argIndex, (IntPtr)size, null);
        }

        #endregion

        #region Event object API

        public static InfoBuffer GetEventInfo(Event e, EventInfo paramName, out ErrorCode error)
        {
            return GetInfo(GetEventInfo, e, paramName, out error);
        }

        #endregion

        #region Sampler API

        public static InfoBuffer GetSamplerInfo(Sampler sampler, SamplerInfo paramName, out ErrorCode error)
        {
            return GetInfo(GetSamplerInfo, sampler, paramName, out error);
        }

        #endregion

        public static InfoBuffer GetEventProfilingInfo(Event e, ProfilingInfo paramName, out ErrorCode error)
        {
            return GetInfo(GetEventProfilingInfo, e, paramName, out error);
        }
    }
}
