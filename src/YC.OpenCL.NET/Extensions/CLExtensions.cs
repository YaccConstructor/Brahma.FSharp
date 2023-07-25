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
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Runtime.InteropServices;
using ExtraConstraints;

namespace OpenCL.Net.Extensions
{
    public static class CLExtensions
    {
        public static Environment CreateCLEnvironment(this string platformWildCard, 
            DeviceType deviceType = DeviceType.Default, 
            CommandQueueProperties commandQueueProperties = CommandQueueProperties.None)
        {
            return new Environment(platformWildCard, deviceType, commandQueueProperties);
        }

        private static Kernel _CompileKernel(this Context context, string source, string kernelName, out string errors, string options = null)
        {
            errors = string.Empty;
            ErrorCode error;
            var devicesInfoBuffer = Cl.GetContextInfo(context, ContextInfo.Devices, out error);
            var devices = devicesInfoBuffer.CastToArray<Device>((devicesInfoBuffer.Size / Marshal.SizeOf(typeof(IntPtr))));
            var program = Cl.CreateProgramWithSource(context, 1, new[] { source }, new[] { (IntPtr)source.Length }, out error);
            error = Cl.BuildProgram(program, (uint)devices.Length, devices, options == null ? string.Empty : options, null, IntPtr.Zero);
            if (error != ErrorCode.Success)
            {
                errors = string.Join("\n", from device in devices
                                            select Cl.GetProgramBuildInfo(program, device, ProgramBuildInfo.Log, out error).ToString());
                return new Kernel();
            }
            return Cl.CreateKernel(program, kernelName, out error);
        }

        public static Kernel CompileKernel(this Context context, string path, string kernelName, out string errors, string options = null)
        {
            errors = string.Empty;
            if (!Path.IsPathRooted(path))
                path = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, path);

            return _CompileKernel(context, File.ReadAllText(path), kernelName, out errors, options);
        }
        public static Kernel CompileKernel(this Context context, string path, string kernelName, string options = null)
        {
            string errors;
            var result = CompileKernel(context, path, kernelName, out errors, options);
            if (!result.IsValid() || !string.IsNullOrEmpty(errors))
                throw new Cl.Exception(ErrorCode.InvalidKernel, errors);

            return result;
        }
        public static Kernel CompileKernelFromSource(this Context context, string source, string kernelName, out string errors, string options = null)
        {
            return _CompileKernel(context, source, kernelName, out errors, options);
        }
        public static Kernel CompileKernelFromSource(this Context context, string source, string kernelName, string options = null)
        {
            string errors;
            var result = CompileKernelFromSource(context, source, kernelName, out errors, options);
            if (!result.IsValid() || !string.IsNullOrEmpty(errors))
                throw new Cl.Exception(ErrorCode.InvalidKernel, errors);

            return result;
        }

        public static ErrorCode Compile(this KernelWrapperBase kernel, out string errors, string options = null)
        {
            return kernel.Compile(kernel.KernelSource, kernel.KernelName, out errors, options);
        }
        public static ErrorCode Compile(this KernelWrapperBase kernel, string options = null)
        {
            return kernel.Compile(kernel.KernelSource, kernel.KernelName, options);
        }
        public static ErrorCode CompileForDebugOnIntelCPU(this KernelWrapperBase kernel, out string errors)
        {
            #warning Compiling kernel for debugging
            Console.WriteLine("Compiling {0} for debugging", kernel.KernelName);

            return kernel.Compile(kernel.KernelSource, kernel.KernelName, out errors, string.Format("-cl-opt-disable -g -s \"{0}\"", kernel.OriginalKernelPath));
        }
        public static ErrorCode CompileForDebugOnIntelCPU(this KernelWrapperBase kernel)
        {
            #warning Compiling kernel for debugging
            Console.WriteLine("Compiling {0} for debugging", kernel.KernelName);

            return kernel.Compile(kernel.KernelSource, kernel.KernelName, string.Format("-cl-opt-disable -g -s \"{0}\"", kernel.OriginalKernelPath));
        }

        public struct KernelArgChain
        {
            internal Kernel Kernel;
            internal uint Count;
        }
        // Any value type
        public static KernelArgChain SetKernelArg<T>(this Kernel kernel, T value) where T : struct
        {
            Cl.SetKernelArg<T>(kernel, 0, value).Check();
            return new KernelArgChain { Kernel = kernel, Count = 0 };
        }
        public static KernelArgChain SetKernelArg<T>(this KernelArgChain argChain, T buffer) where T : struct
        {
            Cl.SetKernelArg<T>(argChain.Kernel, ++argChain.Count, buffer).Check();
            return argChain;
        }

        // IMem
        public static KernelArgChain SetKernelArg(this Kernel kernel, IMem buffer)
        {
            Cl.SetKernelArg(kernel, 0, TypeSize<IntPtr>.Size, buffer).Check();
            return new KernelArgChain { Kernel = kernel, Count = 0 };
        }
        public static KernelArgChain SetKernelArg(this KernelArgChain argChain, IMem buffer)
        {
            Cl.SetKernelArg(argChain.Kernel, ++argChain.Count, TypeSize<IntPtr>.Size, buffer).Check();
            return argChain;
        }

        // Local memory
        public static KernelArgChain SetKernelArg<T>(this Kernel kernel, int length) where T : struct
        {
            Cl.SetKernelArg<T>(kernel, 0, length).Check();
            return new KernelArgChain { Kernel = kernel, Count = 0 };
        }
        public static KernelArgChain SetKernelArg<T>(this KernelArgChain argChain, int length) where T : struct
        {
            var size = TypeSize<T>.SizeInt * length;
            Cl.SetKernelArg<T>(argChain.Kernel, ++argChain.Count, size);
            return argChain;
        }

        public static Event EnqueueKernel(this CommandQueue commandQueue, Kernel kernel, 
            uint globalWorkSize, 
            uint localWorkSize = 0, 
            params Event[] waitFor)
        {
            Event e;
            Cl.EnqueueNDRangeKernel(commandQueue, kernel, 1, null,
                new[] { (IntPtr)globalWorkSize },
                localWorkSize == 0 ? null : new[] { (IntPtr)localWorkSize },
                (uint)waitFor.Length, waitFor.Length == 0 ? null : waitFor, out e).Check();
            return e;
        }
        public static Event EnqueueKernel(this CommandQueue commandQueue, Kernel kernel, 
            uint globalWorkSize0, uint globalWorkSize1, 
            uint localWorkSize0 = 0, uint localWorkSize1 = 0, 
            params Event[] waitFor)
        {
            Event e;
            Cl.EnqueueNDRangeKernel(commandQueue, kernel, 2, null,
                new[] { (IntPtr)globalWorkSize0, (IntPtr)globalWorkSize1 },
                (localWorkSize0 == 0) && (localWorkSize1 == 0) ? 
                    null : 
                    new[] { (IntPtr)localWorkSize0, (IntPtr)localWorkSize1 },
                (uint)waitFor.Length, waitFor.Length == 0 ? null : waitFor, out e).Check();
            return e;
        }
        public static Event EnqueueKernel(this CommandQueue commandQueue, Kernel kernel,
            uint globalWorkSize0, uint globalWorkSize1, uint globalWorkSize2,
            uint localWorkSize0 = 0, uint localWorkSize1 = 0, uint localWorkSize2 = 0,
            params Event[] waitFor)
        {
            Event e;
            Cl.EnqueueNDRangeKernel(commandQueue, kernel, 1, null,
                new[] { (IntPtr)globalWorkSize0, (IntPtr)globalWorkSize1, (IntPtr)globalWorkSize2 },
                (localWorkSize0 == 0) && (localWorkSize1 == 0) && (localWorkSize2 == 0) ?
                    null :
                    new[] { (IntPtr)localWorkSize0, (IntPtr)localWorkSize1, (IntPtr)localWorkSize2 },
                (uint)waitFor.Length, waitFor.Length == 0 ? null : waitFor, out e).Check();
            return e;
        }

        public static void EnqueueWaitForEvents(this CommandQueue commandQueue, params Event[] waitFor)
        {
            Cl.EnqueueWaitForEvents(commandQueue, (uint)waitFor.Length, waitFor);
        }
        public static Event EnqueueWriteToBuffer<T>(this CommandQueue commandQueue, IMem buffer, T[] data, int offset = 0, long length = -1, params Event[] waitFor)
            where T : struct
        {
            Event e;
            var elemSize = TypeSize<T>.SizeInt;
            Cl.EnqueueWriteBuffer(commandQueue, buffer, Bool.False, (IntPtr)(offset * elemSize), (IntPtr)((length == -1 ? data.Length : length) * elemSize), data, 
                (uint)waitFor.Length, waitFor.Length == 0 ? null : waitFor, out e)
                .Check();
            return e;
        }
        public static Event EnqueueWriteToBuffer<T>(this CommandQueue commandQueue, IMem<T> buffer, T[] data, int offset = 0, long length = -1, params Event[] waitFor)
            where T : struct
        {
            return EnqueueWriteToBuffer(commandQueue, (IMem)buffer, data, offset, length, waitFor);
        }
        public static void WriteToBuffer<T>(this CommandQueue commandQueue, IMem buffer, T[] data, int offset = 0, long length = -1, params Event[] waitFor)
            where T: struct
        {
            Event e;
            var elemSize = TypeSize<T>.SizeInt;
            Cl.EnqueueWriteBuffer(commandQueue, buffer, Bool.True, (IntPtr)(offset * elemSize), (IntPtr)((length == -1 ? data.Length : length) * elemSize), data,
                (uint)waitFor.Length, waitFor.Length == 0 ? null : waitFor, out e)
                .Check();
            e.Dispose();
        }
        public static void WriteToBuffer<T>(this CommandQueue commandQueue, IMem<T> buffer, T[] data, int offset = 0, long length = -1, params Event[] waitFor)
            where T : struct
        {
            WriteToBuffer(commandQueue, (IMem) buffer, data, offset, length, waitFor);
        }

        public static Event EnqueueReadFromBuffer<T>(this CommandQueue commandQueue, IMem buffer, T[] array, int offset = 0, long length = -1, params Event[] waitFor)
            where T : struct
        {
            Event e;
            var elemSize = TypeSize<T>.SizeInt;
            Cl.EnqueueReadBuffer(commandQueue, buffer, Bool.False, (IntPtr)(offset * elemSize), (IntPtr)((length == -1 ? array.Length : length) * elemSize), array,
                (uint)waitFor.Length, waitFor.Length == 0 ? null : waitFor, out e)
                .Check();

            return e;
        }
        public static Event EnqueueReadFromBuffer<T>(this CommandQueue commandQueue, IMem<T> buffer, T[] array, int offset = 0, long length = -1, params Event[] waitFor)
            where T : struct
        {
            return EnqueueReadFromBuffer(commandQueue, (IMem) buffer, array, offset, length, waitFor);
        }
        public static void ReadFromBuffer<T>(this CommandQueue commandQueue, IMem buffer, T[] array, int offset = 0, long length = -1, params Event[] waitFor)
            where T : struct
        {
            Event e;
            var elemSize = TypeSize<T>.SizeInt;
            Cl.EnqueueReadBuffer(commandQueue, buffer, Bool.True, (IntPtr)(offset * elemSize), (IntPtr)((length == -1 ? array.Length : length) * elemSize), array,
                (uint)waitFor.Length, waitFor.Length == 0 ? null : waitFor, out e)
                .Check();

            e.Dispose();
        }
        public static void ReadFromBuffer<T>(this CommandQueue commandQueue, IMem<T> buffer, T[] array, int offset = 0, long length = -1, params Event[] waitFor)
            where T : struct
        {
            ReadFromBuffer(commandQueue, (IMem)buffer, array, offset, length, waitFor);
        }

        public static ErrorCode Flush(this CommandQueue commandQueue)
        {
            return Cl.Flush(commandQueue);
        }
        public static ErrorCode Wait(this Event ev, bool dispose = true)
        {
            var result = Cl.WaitForEvents(1, new[] { ev });
            if (dispose)
                ev.Dispose();
            return result;
        }
        public static ErrorCode Finish(this CommandQueue commandQueue)
        {
            return Cl.Finish(commandQueue);
        }

        public static IMem<T> CreateBuffer<T>(this Context context, int length, MemFlags flags = MemFlags.None, bool zero = false) where T: struct
        {
            ErrorCode err;
            if (zero)
            {
                var hostData = new T[length];

                var result = Cl.CreateBuffer<T>(context, flags | MemFlags.CopyHostPtr, hostData, out err);

                err.Check();
                hostData = null;

                return result;
            }
            else
            {
                var result = Cl.CreateBuffer<T>(context, flags, length, out err);
                err.Check();

                return result;
            }
        }
        public static IMem<T> CreateBuffer<T>(this Context context, T[] data, MemFlags flags = MemFlags.None) where T : struct
        {
            ErrorCode err;
            var result = Cl.CreateBuffer<T>(context, flags | MemFlags.CopyHostPtr, data, out err);
            err.Check();

            return result;
        }

        public static IMem<TOrder, TType> CreateImage<[EnumConstraint(typeof(ChannelOrder))] TOrder, [EnumConstraint(typeof(ChannelType))] TType, TData>(this Context context, int width, int height, TData[] data)
        { 
            // return 
            return null;
        }
    }
}
