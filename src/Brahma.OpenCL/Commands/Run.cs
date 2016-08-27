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

using System.Collections.Generic;
using System.Linq;
using Brahma.Commands;
using OpenCL.Net;
using ClNet = OpenCL.Net;
using System;
using System.Runtime.InteropServices;

namespace Brahma.OpenCL.Commands
{
    public abstract class RunBase<TRange> : Brahma.Commands.Run<TRange>
        where TRange: struct, INDRangeDimension
    {
        private static readonly IntPtr _intPtrSize = (IntPtr)Marshal.SizeOf(typeof(IntPtr));
        private object curArgVal;
        private System.IntPtr curArgSize;
        private ICLKernel kernel;

        private void ArrayToMem(object data, System.Type t)
        {
            curArgSize = _intPtrSize;
            if (kernel.Provider.AutoconfiguredBuffers.ContainsKey(data))
            {
                curArgVal = kernel.Provider.AutoconfiguredBuffers[data];
            }
            else
            {
                ErrorCode error;
                var operations = Operations.ReadWrite;
                var memory = Memory.Device;
                var _elementSize = Marshal.SizeOf(t);
                var mem = Cl.CreateBuffer(kernel.Provider.Context, (MemFlags)operations | (memory == Memory.Host ? MemFlags.UseHostPtr : (MemFlags)memory | MemFlags.CopyHostPtr),
                    (IntPtr)(_elementSize * ((Array)data).Length), data, out error);
                curArgVal = mem;
                //mem.Pin();
                kernel.Provider.AutoconfiguredBuffers.Add(data, (Mem)mem);
                if (error != ErrorCode.Success)
                    throw new CLException(error);
            }                        
        }

        private void ToIMem(object arg)
        {

            var x = arg.GetType();
            //Console.WriteLine(x);
            var isIMem = arg is IMem;
            if (isIMem)
            {
                curArgSize = ((IMem)arg).Size;
                curArgVal = ((IMem)arg).Data;
            }
            else if (arg.GetType().ToString().EndsWith("[]")) ArrayToMem(arg, arg.GetType().GetElementType());            
            else
            {
                curArgSize = (System.IntPtr)System.Runtime.InteropServices.Marshal.SizeOf(arg);
                curArgVal = arg;
            }

        }

        public override void SetupArgument(object sender, int index, object arg)
        {
            kernel = Kernel as ICLKernel;
            ToIMem(arg);            
            ErrorCode error = 
                    Cl.SetKernelArg(kernel.ClKernel, (uint)index
                    , curArgSize
                    , curArgVal);
            if (error != ErrorCode.Success)
                throw new CLException(error);
        }
        
        protected RunBase(IKernel kernel, TRange range)
            : base(kernel, range)
        {
        }

        public override void Execute(object sender)
        {
            base.Execute(sender);

            var queue = sender as CommandQueue;
            var kernel = Kernel as ICLKernel;
            var range = Range as INDRangeDimension;
            var waitList = (from name in WaitList
                            let ev = CommandQueue.FindEvent(name)
                            where ev != null
                            select ev.Value).ToArray();

            Event eventID;
            ErrorCode error = Cl.EnqueueNDRangeKernel(queue.Queue, kernel.ClKernel, (uint)kernel.WorkDim, null,
                range.GlobalWorkSize, range.LocalWorkSize, (uint)waitList.Length, waitList.Length == 0 ? null : waitList.ToArray(), out eventID);
            if (error != ErrorCode.Success)
                throw new CLException(error);

            if (Name == string.Empty)
                eventID.Dispose();
            else
                CommandQueue.AddEvent(Name, eventID);
        }
    }

    public sealed class Run<TRange>: RunBase<TRange>, ICommand<TRange>
        where TRange: struct, INDRangeDimension
    {
        protected override IEnumerable<object> Arguments
        {
            get
            {
                return args;
            }
        }
        
        public Run(IKernel kernel, TRange range)
            : base(kernel, range)
        {
                
        }

        public object[] args
        {
            get;
            internal set;
        }
    }
}