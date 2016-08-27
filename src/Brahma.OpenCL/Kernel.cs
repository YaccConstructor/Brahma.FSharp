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
using System.Linq.Expressions;
using System.Text;
using Brahma.OpenCL.Commands;
using OpenCL.Net;

namespace Brahma.OpenCL
{
    public interface ICLKernel: IKernel
    {

        ComputeProvider Provider
        {
            get;
            set;
        }

        StringBuilder Source
        {
            get;
            set;
        }       

        Kernel ClKernel
        {
            get;
            set;
        }

        int WorkDim
        {
            get;
        }

        void SetClosures(IEnumerable<MemberExpression> closures);
        void SetParameters(IEnumerable<ParameterExpression> parameters);
    }

    public abstract class KernelBase<TRange>: ICLKernel
        where TRange: struct, Brahma.INDRangeDimension
    {        

        private static readonly TRange _range; // Default value will do

        private StringBuilder _source = new StringBuilder();
        private IEnumerable<MemberExpression> _closures;
        private IEnumerable<ParameterExpression> _parameters;


        #region ICLKernel Members
        
        StringBuilder ICLKernel.Source
        {
            get { return _source; }
            set { _source = value; }
        }

        ComputeProvider ICLKernel.Provider
        {
            get;
            set;
        }

        Kernel ICLKernel.ClKernel
        {
            get; set;
        }

        int ICLKernel.WorkDim
        {
            get { return ((INDRangeDimension)_range).Dimensions; }
        }

        void ICLKernel.SetClosures(IEnumerable<MemberExpression> closures)
        {
            _closures = closures;
        }

        void ICLKernel.SetParameters(IEnumerable<ParameterExpression> parameters)
        {
            _parameters = parameters;
        }

        #endregion

        #region IKernel Members

        IEnumerable<MemberExpression> IKernel.Closures
        {
            get { return _closures; }
        }

        IEnumerable<ParameterExpression> IKernel.Parameters
        {
            get { return _parameters; }
        }

        #endregion
    }

    public sealed class Kernel<TRange>: KernelBase<TRange>, IKernel<TRange>
        where TRange: struct, Brahma.INDRangeDimension
    {
    }

    public static class KernelExtensions
    {
        public static Run<TRange> Run<TRange>(this Kernel<TRange> kernel, TRange range, object[] args)
            where TRange : struct, INDRangeDimension
        {
            return new Run<TRange>(kernel, range) { args = args };
        }

        public static Run<TRange> Run<TRange>(this Kernel<TRange> kernel, TRange range)
            where TRange: struct, INDRangeDimension
        {
            return new Run<TRange>(kernel, range);
        }
        
    }
}