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

namespace Brahma.OpenCL
{
    public sealed class NDRange<T>: Brahma.NDRange<T> where T: struct, Brahma.INDRangeDimension
    {
        private readonly T _range;

        internal NDRange()
        {
            _range = new T();
        }
        
        public NDRange(T range)
        {
            _range = range;
        }
        
        public override IEnumerator<T> GetEnumerator()
        {
            throw new NotSupportedException();
        }

        public T Range
        {
            get
            {
                return _range;
            }
        }
    }
}