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
using System.Collections;
using System.Diagnostics;
using System.Linq;
using System.Runtime.InteropServices;
using System.Runtime.Serialization;
using System.Collections.Generic;
using ExtraConstraints;

namespace OpenCL.Net
{
    public static partial class Cl
    {
        [Serializable]
        public class Exception : System.Exception
        {
            public Exception(ErrorCode error)
                : base(error.ToString())
            {
            }

            public Exception(ErrorCode error, string message)
                : base(string.Format("{0}: {1}", error, message))
            {
            }

            public Exception(ErrorCode error, string message, Exception inner)
                : base(string.Format("{0}: {1}", error, message), inner)
            {
            }

            protected Exception(
                SerializationInfo info,
                StreamingContext context)
                : base(info, context)
            {
            }
        }
    }

    public sealed class Environment: IDisposable
    {
        public Context Context { get; private set; }
        public CommandQueue[] CommandQueues { get; private set; }
        public Device[] Devices { get; private set; }
        public DeviceType[] DeviceTypes { get; private set; }

        public Environment(string platformWildCard, DeviceType deviceType = DeviceType.Default,
            CommandQueueProperties commandQueueProperties = CommandQueueProperties.None)
        {
            ErrorCode error;

            Context = Cl.CreateContext(platformWildCard, deviceType, out error);
            error.Check();

            var deviceInfoBuffer = Cl.GetContextInfo(Context, ContextInfo.Devices, out error);
            error.Check();
                
            Devices = deviceInfoBuffer.CastToArray<Device>(deviceInfoBuffer.Size / Marshal.SizeOf(typeof(IntPtr)));
                
            DeviceTypes = (from d in Devices select Cl.GetDeviceInfo(d, DeviceInfo.Type, out error).CastTo<DeviceType>()).ToArray();
            error.Check();

            CommandQueues = (from d in Devices select Cl.CreateCommandQueue(Context, d, commandQueueProperties, out error)).ToArray();
            error.Check();
        }

        public void Dispose()
        {
            foreach (var commandQueue in CommandQueues)
                commandQueue.Dispose();
            CommandQueues = null;

            Context.Dispose();
            Context = new Context(); // Invalid
        }
    }

    public sealed class TypeSize<T>
    {
        public static readonly IntPtr Size = (IntPtr)Marshal.SizeOf(typeof(T));
        public static readonly int SizeInt = Marshal.SizeOf(typeof(T));
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct Sampler : IRefCountedHandle
    {
        private readonly IntPtr _handle;

        internal Sampler(IntPtr handle)
        {
            _handle = handle;
        }

        #region IRefCountedHandle Members

        public void Retain()
        {
            Cl.RetainSampler(this);
        }

        public void Release()
        {
            Cl.ReleaseSampler(this);
        }

        #endregion

        #region IHandleData Members

        IntPtr IHandleData.Handle
        {
            get
            {
                return _handle;
            }
        }

        #endregion

        #region IDisposable Members

        public void Dispose()
        {
            Release();
        }

        #endregion
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct Event : IRefCountedHandle
    {
        private readonly IntPtr _handle;

        internal Event(IntPtr handle)
        {
            _handle = handle;
        }

        #region IRefCountedHandle Members

        public void Retain()
        {
            Cl.RetainEvent(this);
        }

        public void Release()
        {
            Cl.ReleaseEvent(this);
        }

        #endregion

        #region IHandleData Members

        IntPtr IHandleData.Handle
        {
            get
            {
                return _handle;
            }
        }

        #endregion

        #region IDisposable Members

        public void Dispose()
        {
            Release();
        }

        #endregion
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct Kernel : IRefCountedHandle
    {
        private readonly IntPtr _handle;

        internal Kernel(IntPtr handle)
        {
            _handle = handle;
        }

        #region IRefCountedHandle Members

        public void Retain()
        {
            Cl.RetainKernel(this);
        }

        public void Release()
        {
            Cl.ReleaseKernel(this);
        }

        #endregion

        #region IHandleData Members

        IntPtr IHandleData.Handle
        {
            get
            {
                return _handle;
            }
        }

        #endregion

        #region IDisposable Members

        public void Dispose()
        {
            Release();
        }

        #endregion
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct CommandQueue : IRefCountedHandle
    {
        private readonly IntPtr _handle;

        internal CommandQueue(IntPtr handle)
        {
            _handle = handle;
        }
            
        #region IRefCountedHandle Members

        public void Retain()
        {
            Cl.RetainCommandQueue(this);
        }

        public void Release()
        {
            Cl.ReleaseCommandQueue(this);
        }

        #endregion

        #region IHandleData Members

        IntPtr IHandleData.Handle
        {
            get
            {
                return _handle;
            }
        }

        #endregion

        #region IDisposable Members

        public void Dispose()
        {
            Release();
        }

        #endregion
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct Program : IRefCountedHandle
    {
        private readonly IntPtr _handle;

        internal Program(IntPtr handle)
        {
            _handle = handle;
        }

        #region IRefCountedHandle Members

        public void Retain()
        {
            Cl.RetainProgram(this);
        }

        public void Release()
        {
            Cl.ReleaseProgram(this);
        }

        #endregion

        #region IHandleData Members

        IntPtr IHandleData.Handle
        {
            get
            {
                return _handle;
            }
        }

        #endregion

        #region IDisposable Members

        public void Dispose()
        {
            Release();
        }

        #endregion
    }

    [DebuggerTypeProxy(typeof(MemDebugView<>))]
    public struct Mem<T> : IMem<T>, IRefCountedHandle, IEquatable<Mem<T>>
        where T : struct
    {
        private readonly IntPtr _handle;

        internal Mem(IntPtr handle)
        {
            _handle = handle;
        }

        internal Mem(IMem mem)
        {
            _handle = ((IHandleData)mem).Handle;
        }

        #region IRefCountedHandle Members

        public void Retain()
        {
            Cl.RetainMemObject(this);
        }

        public void Release()
        {
            Cl.ReleaseMemObject(this);
        }

        #endregion

        #region IHandleData Members

        IntPtr IHandleData.Handle
        {
            get
            {
                return _handle;
            }
        }

        #endregion

        #region IDisposable Members

        public void Dispose()
        {
            Release();
        }

        #endregion

        public bool Equals(Mem<T> other)
        {
            return _handle.ToInt64() == other._handle.ToInt64();
        }

    }

    internal sealed class MemDebugView<TElem>
        where TElem : struct
    {
        private readonly Mem<TElem> _mem;

        public MemDebugView(Mem<TElem> mem)
        {
            _mem = mem;
        }

        public TElem[] Values
        {
            get
            {
                ErrorCode err;
                var size = Cl.GetMemObjectInfo(_mem, MemInfo.Size, out err).CastTo<IntPtr>().ToInt64();
                err.Check();

                var elemSize = TypeSize<TElem>.SizeInt;
                var length = size / elemSize;
                var result = new TElem[length];

                var context = Cl.GetMemObjectInfo(_mem, MemInfo.Context, out err).CastTo<Context>();
                err.Check();

                var devicesInfoBuffer = Cl.GetContextInfo(context, ContextInfo.Devices, out err);
                err.Check();
                var devices = devicesInfoBuffer.CastToArray<Device>((devicesInfoBuffer.Size / Marshal.SizeOf(typeof(IntPtr))));
                var commandQueue = Cl.CreateCommandQueue(context, devices.First(), CommandQueueProperties.None, out err);

                Event ev;
                Cl.EnqueueReadBuffer(commandQueue, _mem, Bool.True, result, 0, null, out ev).Check();
                ev.Dispose();

                return result;
            }
        }
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct Mem : IMem, IRefCountedHandle, IEquatable<Mem>
    {
        private readonly IntPtr _handle;

        internal Mem(IntPtr handle)
        {
            _handle = handle;
        }

        #region IRefCountedHandle Members

        public void Retain()
        {
            Cl.RetainMemObject(this);
        }

        public void Release()
        {
            Cl.ReleaseMemObject(this);
        }

        #endregion

        #region IHandleData Members

        IntPtr IHandleData.Handle
        {
            get
            {
                return _handle;
            }
        }

        #endregion

        #region IDisposable Members

        public void Dispose()
        {
            Release();
        }

        #endregion

        public bool Equals(Mem other)
        {
            return _handle.ToInt64() == other._handle.ToInt64();
        }
    }

    public interface IMem<T> : IMem
        where T: struct
    { }

    public interface IMem<[EnumConstraint(typeof(ChannelOrder))] TOrder, [EnumConstraint(typeof(ChannelType))] TType>: IMem
    {
    }

    public interface IMem: IDisposable
    { 
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct Context : IRefCountedHandle
    {
        private readonly IntPtr _handle;

        internal Context(IntPtr handle)
        {
            _handle = handle;
        }

        #region IHandleData Members

        IntPtr IHandleData.Handle
        {
            get
            {
                return _handle;
            }
        }

        #endregion

        #region IRefCountedHandle Members

        public void Retain()
        {
            Cl.RetainContext(this);
        }

        public void Release()
        {
            Cl.ReleaseContext(this);
        }

        #endregion

        #region IDisposable Members

        public void Dispose()
        {
            Release();
        }

        #endregion

        public static readonly Context Zero = new Context(IntPtr.Zero);
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct ContextProperty
    {
        private static readonly ContextProperty _zero = new ContextProperty(0);

        private readonly uint _propertyName;
        private readonly IntPtr _propertyValue;

        public ContextProperty(ContextProperties property, IntPtr value)
        {
            _propertyName = (uint)property;
            _propertyValue = value;
        }

        public ContextProperty(ContextProperties property)
        {
            _propertyName = (uint)property;
            _propertyValue = IntPtr.Zero;
        }

        public static ContextProperty Zero
        {
            get
            {
                return _zero;
            }
        }
    }

    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    public struct ImageFormat
    {
        [MarshalAs(UnmanagedType.U4)]
        private ChannelOrder _channelOrder;
        [MarshalAs(UnmanagedType.U4)]
        private ChannelType _channelType;

        public ImageFormat(ChannelOrder channelOrder, ChannelType channelType)
        {
            _channelOrder = channelOrder;
            _channelType = channelType;
        }

        public ChannelOrder ChannelOrder
        {
            get
            {
                return _channelOrder;
            }
            set
            {
                _channelOrder = value;
            }
        }

        public ChannelType ChannelType
        {
            get
            {
                return _channelType;
            }
            set
            {
                _channelType = value;
            }
        }
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct Device : IHandle, IHandleData
    {
        private readonly IntPtr _handle;

        internal Device(IntPtr handle)
        {
            _handle = handle;
        }

        #region IHandleData Members

        IntPtr IHandleData.Handle
        {
            get
            {
                return _handle;
            }
        }

        #endregion
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct Platform : IHandle, IHandleData
    {
        private readonly IntPtr _handle;

        internal Platform(IntPtr handle)
        {
            _handle = handle;
        }

        #region IHandleData Members

        IntPtr IHandleData.Handle
        {
            get
            {
                return _handle;
            }
        }

        #endregion
    }

    internal interface IRefCountedHandle : IHandle, IHandleData, IDisposable
    {
        void Retain();
        void Release();
    }

    internal interface IHandleData
    {
        IntPtr Handle
        {
            get;
        }
    }

    public interface IHandle
    {
    }

    public struct PinnedObject : IDisposable
    {
        private readonly GCHandle _handle;

        internal PinnedObject(object obj)
        {
            _handle = GCHandle.Alloc(obj, GCHandleType.Pinned);
        }

        public void Unpin()
        {
            Dispose();
        }

        #region IDisposable Members

        public void Dispose()
        {
            _handle.Free();
        }

        #endregion

        public static implicit operator IntPtr(PinnedObject pinned)
        {
            return pinned._handle.AddrOfPinnedObject();
        }
    }

    public sealed class ArraySegment<T> : IEnumerable<T>
    {
        private readonly T[] _array;
        private readonly int _index;
        private readonly int _count;

        internal ArraySegment(T[] array, int index, int count)
        {
            if (array == null)
                throw new ArgumentNullException("array");
            if (index > array.Length)
                throw new IndexOutOfRangeException("index");
            if (index + count > array.Length)
                throw new ArgumentOutOfRangeException("count");

            _index = index;
            _count = count;

            _array = array;
        }

        public T this[int index]
        {
            get
            {
                if ((index < 0) || (index > _count) || (_index + index > _array.Length))
                    throw new IndexOutOfRangeException("index");

                return _array[_index + index];
            }
        }

        public int Length
        {
            get
            {
                return _count;
            }
        }

        public IEnumerator<T> GetEnumerator()
        {
            for (int i = _index; i < _count; i++)
                yield return _array[i];
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        public static implicit operator T[](ArraySegment<T> segment)
        {
            return segment._array;
        }

        public static implicit operator ArraySegment<T>(T[] array)
        {
            return new ArraySegment<T>(array, 0, array.Length);
        }
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct InfoBufferArray<T> : IDisposable
        where T : struct
    {
        private static readonly IntPtr size = typeof(T).IsEnum ? (IntPtr)Marshal.SizeOf(Enum.GetUnderlyingType(typeof(T))) : (IntPtr)Marshal.SizeOf(typeof(T));

        private readonly IntPtr[] _buffers;

        internal IntPtr[] Array
        {
            get
            {
                return _buffers;
            }
        }

        public InfoBufferArray(int length)
        {
            _buffers = new IntPtr[length];
            for (int i = 0; i < length; i++)
                _buffers[i] = new InfoBuffer(size).Address;
        }

        public T this[int index]
        {
            get
            {
                return new InfoBuffer
                {
                    Address = _buffers[index]
                }.CastTo<T>();
            }
        }

        public int Length
        {
            get
            {
                return _buffers.Length;
            }
        }

        public IntPtr Size
        {
            get
            {
                return (IntPtr)(_buffers.Length * IntPtr.Size);
            }
        }

        public void Dispose()
        {
            for (int i = 0; i < _buffers.Length; i++)
                new InfoBuffer
                {
                    Address = _buffers[i]
                }.Dispose();
        }
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct InfoBufferArray : IDisposable
    {
        private readonly IntPtr[] _buffers;

        internal IntPtr[] Array
        {
            get
            {
                return _buffers;
            }
        }

        public InfoBufferArray(params InfoBuffer[] buffers)
        {
            _buffers = new IntPtr[buffers.Length];
            for (int i = 0; i < buffers.Length; i++)
                _buffers[i] = buffers[i].Address;
        }

        public InfoBuffer this[int index]
        {
            get
            {
                return new InfoBuffer
                {
                    Address = _buffers[index]
                };
            }
        }

        public int Length
        {
            get
            {
                return _buffers.Length;
            }
        }

        public IntPtr Size
        {
            get
            {
                return (IntPtr)(_buffers.Length * IntPtr.Size);
            }
        }

        public void Dispose()
        {
            for (int i = 0; i < _buffers.Length; i++)
                new InfoBuffer
                {
                    Address = _buffers[i]
                }.Dispose();
        }
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct InfoBuffer : IDisposable
    {
        [DllImport("msvcrt.dll", EntryPoint = "memcpy")]
        private static extern void CopyMemory(IntPtr pDest, IntPtr pSrc, int length);

        private static readonly InfoBuffer _empty = new InfoBuffer
        {
            _buffer = IntPtr.Zero
        };

        private IntPtr _buffer;
        private int _size;

        public InfoBuffer(IntPtr size)
        {
            _size = (int)size;
            _buffer = Marshal.AllocHGlobal(size);
        }

        public InfoBuffer(byte[] array)
        {
            int length = array.Length;
            _size = length;
            _buffer = Marshal.AllocHGlobal(length);
            using (var source = array.Pin())
                CopyMemory(_buffer, source, length);
        }

        internal IntPtr Address
        {
            get
            {
                return _buffer;
            }
            set
            {
                _buffer = value;
            }
        }

        public T CastTo<T>() where T : struct
        {
            return _buffer.ElementAt<T>(0);
        }

        public T CastTo<T>(int index) where T : struct
        {
            return _buffer.ElementAt<T>(index);
        }

        public IEnumerable<T> CastToEnumerable<T>(IEnumerable<int> indices) where T : struct
        {
            foreach (int index in indices)
                yield return _buffer.ElementAt<T>(index);
        }

        public T[] CastToArray<T>(int length) where T : struct
        {
            var result = new T[length];
            for (int i = 0; i < length; i++)
                result[i] = _buffer.ElementAt<T>(i);

            return result;
        }

        public int Size { get { return _size; } }

        public override string ToString()
        {
            return Marshal.PtrToStringAnsi(_buffer);
        }

        public static InfoBuffer Empty
        {
            get
            {
                return _empty;
            }
        }

        #region IDisposable Members

        public void Dispose()
        {
            if (_buffer != IntPtr.Zero)
            {
                Marshal.FreeHGlobal(_buffer);
                _buffer = IntPtr.Zero;
            }
        }

        #endregion
    }
}
