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
using System.Runtime.InteropServices;

namespace OpenCL.Net.Extensions
{
    public abstract class KernelWrapperBase
    {
        private Kernel _kernel;
        private readonly Context _context;
        
        protected KernelWrapperBase(Context context)
        {
            _context = context;
        }

        protected uint GetWorkDimension(uint x, uint y, uint z)
        {
            return (uint)((x > 0 ? 1 : 0) + (y > 0 ? 1 : 0) + (z > 0 ? 1 : 0));
        }

        protected IntPtr[] GetWorkSizes(uint x, uint y, uint z)
        {
            var sum = GetWorkDimension(x, y, z);
            switch (sum)
            {
                case 0:
                    return null;

                case 1:
                    return new[] { (IntPtr)x };

                case 2:
                    return new[] { (IntPtr)x, (IntPtr)y };

                case 3:
                    return new[] { (IntPtr)x, (IntPtr)y, (IntPtr)z };

                default:
                    throw new Cl.Exception(ErrorCode.InvalidWorkDimension);
            }
        }

        internal ErrorCode Compile(string source, string kernelName, out string errors, string options = null)
        {
            errors = string.Empty;
            ErrorCode error;
            var devicesInfoBuffer = Cl.GetContextInfo(_context, ContextInfo.Devices, out error);
            var devices = devicesInfoBuffer.CastToArray<Device>((devicesInfoBuffer.Size / Marshal.SizeOf(typeof(Device))));
            var program = Cl.CreateProgramWithSource(_context, 1, new[] { source }, new[] { (IntPtr)source.Length }, out error);
            error = Cl.BuildProgram(program, (uint)devices.Length, devices, options == null ? string.Empty : options, null, IntPtr.Zero);
            if (error != ErrorCode.Success)
            {
                errors = string.Join("\n", from device in devices
                                            select Cl.GetProgramBuildInfo(program, device, ProgramBuildInfo.Log, out error).ToString());
                throw new Cl.Exception(error, errors);
            }
            _kernel = Cl.CreateKernel(program, kernelName, out error);
            return error;
        }

        internal ErrorCode Compile(string kernelSource, string kernelName, string options = null)
        {
            string errors;
            var result = Compile(kernelSource, kernelName, out errors, options);
            if (result != ErrorCode.Success)
                throw new Cl.Exception(result, errors);

            return result;
        }

        protected internal abstract string KernelPath { get; }
        protected internal abstract string OriginalKernelPath { get; }
        protected internal abstract string KernelSource { get; }
        protected internal abstract string KernelName { get; }

        public Context Context { get { return _context; } }
        public Kernel Kernel { get { return _kernel; } }
    }
}
