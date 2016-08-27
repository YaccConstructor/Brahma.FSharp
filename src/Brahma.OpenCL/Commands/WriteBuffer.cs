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
using OpenCL.Net;

namespace Brahma.OpenCL.Commands
{
    public sealed class WriteBuffer<T>: Brahma.Commands.Command 
    {
        public WriteBuffer(
           Mem mem,
           int elemSize,
           bool blocking,
           int offset,
           int count,
           Array data)
        {
            ElementSize = elemSize;
            Mem = mem;
            Blocking = blocking;
            Offset = offset;
            Count = count;
            Data = data;
            DataPtr = IntPtr.Zero;
        }

        private WriteBuffer(Buffer<T> buffer,
            bool blocking,
            int offset,
            int count,
            Array data,
            IntPtr dataPtr)
        {
            Buffer = buffer;
            Blocking = blocking;
            Offset = offset;
            Count = count;
            Data = data;
            DataPtr = dataPtr;
        }

        internal WriteBuffer(Buffer<T> buffer,
                             bool blocking,
                             int offset,
                             int count,
                             Array data)
            : this(buffer, blocking, offset, count, data, IntPtr.Zero)
        {
        }

        internal WriteBuffer(Buffer<T> buffer,
                           bool blocking,
                           int offset,
                           int count,
                           T[] data)
            : this(buffer, blocking, offset, count, (Array)data)
        {
        }

        internal WriteBuffer(Buffer<T> buffer,
                           bool blocking,
                           int offset,
                           int count,
                           IntPtr data)
            : this(buffer, blocking, offset, count, null, data)
        {
        }

        private Mem Mem
        {
            get;
            set;
        }

        private int ElementSize
        {
            get;
            set;
        }

        private Buffer<T> Buffer
        {
            get;
            set;
        }

        private bool Blocking
        {
            get;
            set;
        }

        private int Offset
        {
            get;
            set;
        }

        private int Count
        {
            get;
            set;
        }

        private Array Data
        {
            get;
            set;
        }

        private IntPtr DataPtr
        {
            get;
            set;
        }

        public override void Execute(object sender)
        {
            var commandQueue = sender as CommandQueue;

            var waitList = from name in WaitList
                           let ev = CommandQueue.FindEvent(name)
                           where ev != null
                           select ev.Value;

            Event eventID;
            ErrorCode error;            
            var mem = (Buffer == null) ? Mem : Buffer.Mem;
            var elementSize = (Buffer == null) ? ElementSize : Buffer.ElementSize;
            if (Data == null)
                error = Cl.EnqueueWriteBuffer(commandQueue.Queue, mem,
                    Blocking ? Bool.True : Bool.False, (IntPtr)Offset,
                    (IntPtr)(Count * elementSize), DataPtr, (uint)waitList.Count(), waitList.Count() == 0? null : waitList.ToArray(), out eventID);
            else
                error = Cl.EnqueueWriteBuffer(commandQueue.Queue, mem,
                    Blocking ? Bool.True : Bool.False, (IntPtr)Offset,
                    (IntPtr)(Count * elementSize), Data, (uint)waitList.Count(), waitList.Count() == 0 ? null : waitList.ToArray(), out eventID);

            if (error != ErrorCode.Success)
                throw new Cl.Exception(error);

            if (Name == string.Empty)
                eventID.Dispose();
            else
                CommandQueue.AddEvent(Name, eventID);
        }
    }
}