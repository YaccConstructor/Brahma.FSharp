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
using System.Linq.Expressions;
using System.Reflection;
using System.Runtime.InteropServices;

namespace Brahma.Commands
{
    public abstract class Run<TRange>: Command, ICommand<TRange>
        where TRange: struct, INDRangeDimension
    {
        protected Run(IKernel kernel, TRange range)
        {
            Kernel = kernel;
            Range = range;
        }

        protected abstract IEnumerable<object> Arguments
        {
            get;
        }

        public abstract void SetupArgument(object sender, int index, object argument);

        public override void Execute(object sender)
        {
            int index = 0;

            foreach (var argument in Arguments)
                SetupArgument(sender, index++, argument);

        }

        public TRange Range
        {
            get;
            private set;
        }

        public IKernel Kernel
        {
            get;
            private set;
        }
    }
}