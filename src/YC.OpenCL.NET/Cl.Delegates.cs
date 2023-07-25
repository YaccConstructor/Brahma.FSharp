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
    public static partial class Cl
    {
        public delegate void ContextNotify(string errInfo, byte[] data, IntPtr cb, IntPtr userData);
        public delegate void ProgramNotify(Program program, IntPtr userData);
        public delegate void NativeKernel(IntPtr args);

        public delegate ErrorCode RetainHandleHandler(IntPtr handle);
        public delegate ErrorCode ReleaseHandleHandler(IntPtr handle);

        public delegate ErrorCode GetInfoDelegate<in THandleType, in TEnumType>(THandleType handle,
                                                                          TEnumType infoName,
                                                                          IntPtr paramValueSize,
                                                                          InfoBuffer buffer,
                                                                          out IntPtr paramValueSizeRet);
        public delegate ErrorCode GetInfoDelegate<in THandle1Type, in THandle2Type, in TEnumType>(THandle1Type handle1,
                                                                                         THandle2Type handle2,
                                                                                         TEnumType infoName,
                                                                                         IntPtr paramValueSize,
                                                                                         InfoBuffer buffer,
                                                                                         out IntPtr paramValueSizeRet);
    }
}
