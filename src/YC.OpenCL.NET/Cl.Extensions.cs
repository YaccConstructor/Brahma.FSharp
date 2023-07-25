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
using System.Runtime.InteropServices;

namespace OpenCL.Net
{
    public static partial class Cl
    {
        internal static InfoBuffer GetInfo<THandleType, TEnumType>(
            GetInfoDelegate<THandleType, TEnumType> method, THandleType handle, TEnumType name, out ErrorCode error)
        {
            IntPtr paramSize;
            error = method(handle, name, IntPtr.Zero, InfoBuffer.Empty, out paramSize);
            // no error checking here because some implementations return InvalidValue even
            // though the paramSize is correctly returned

            var buffer = new InfoBuffer(paramSize);
            error = method(handle, name, paramSize, buffer, out paramSize);
            if (error != ErrorCode.Success)
                return InfoBuffer.Empty;

            return buffer;
        }

        internal static InfoBuffer GetInfo<THandle1Type, THandle2Type, TEnumType>(
            GetInfoDelegate<THandle1Type, THandle2Type, TEnumType> method, THandle1Type handle1, THandle2Type handle2, TEnumType name, out ErrorCode error)
        {
            IntPtr paramSize;
            error = method(handle1, handle2, name, IntPtr.Zero, InfoBuffer.Empty, out paramSize);
            // no error checking here because some implementations return InvalidValue even
            // though the paramSize is correctly returned

            var buffer = new InfoBuffer(paramSize);
            error = method(handle1, handle2, name, paramSize, buffer, out paramSize);
            if (error != ErrorCode.Success)
                return InfoBuffer.Empty;

            return buffer;
        }

        // TODO: Figure out how to use segments of large arrays well ...
        // Or should we ditch this problem and let the user handle it?

        public static IntPtr Increment(this IntPtr ptr, int cbSize)
        {
            return new IntPtr(ptr.ToInt64() + cbSize);
        }

        public static IntPtr Increment<T>(this IntPtr ptr)
        {
            return ptr.Increment(Marshal.SizeOf(typeof(T)));
        }

        public static T ElementAt<T>(this IntPtr ptr, int index) where T: struct
        {
            Type resultType = typeof(T);
            resultType = resultType.IsEnum ? Enum.GetUnderlyingType(resultType) : resultType;
            
            var offset = Marshal.SizeOf(resultType) * index;
            var offsetPtr = ptr.Increment(offset);

            return (T)Marshal.PtrToStructure(offsetPtr, resultType);
        }

        public static ErrorCode OnError(this ErrorCode error, ErrorCode errorCode, Action<ErrorCode> action)
        {
            if (error == errorCode)
                action(error);

            return error;
        }

        public static ErrorCode OnAnyError(this ErrorCode error, Action<ErrorCode> action)
        {
            if (error != ErrorCode.Success)
                action(error);

            return error;
        }

        public static ErrorCode Check(this ErrorCode error)
        {
            error.OnAnyError(e =>
            {
                throw new Cl.Exception(e);
            });
            return error;
        }

        public static PinnedObject Pin(this object obj)
        {
            return new PinnedObject(obj);
        }

        public static T[] InitializeArray<T>(this T[] arr) where T : new()
        {
            for (int i = 0; i < arr.Length; i++)
                arr[i] = new T();

            return arr;
        }

        public static bool IsValid(this IHandle handle)
        {
            return ((IHandleData)handle).Handle != IntPtr.Zero;
        }

        public static IMem<T2> Cast<T1, T2>(this IMem<T1> mem)
            where T1: struct
            where T2: struct
        {
            return new Mem<T2>((mem as IHandleData).Handle);
        }

        public static uint GetPaddedGlobalWorkSize(this uint workSize, uint localWorkSize)
        {
            return (localWorkSize * (uint)(Math.Ceiling((float)workSize / localWorkSize)));
        }
    }
}
