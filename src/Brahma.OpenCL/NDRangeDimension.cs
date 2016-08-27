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

namespace Brahma.OpenCL
{
    public interface INDRangeDimension : Brahma.INDRangeDimension
    {
        IntPtr[] GlobalWorkSize
        {
            get;
        }

        IntPtr[] LocalWorkSize
        {
            get;
        }
    }
    
    public struct _1D : INDRangeDimension
    {
        public struct IDs_1D
        {
            private int _x;
            
            internal IDs_1D(int x)
            {
                _x = x;
            }
            
            
            public int x
            {
                get
                {
                    return _x;
                }
            }
        }

        private IDs_1D _globalIDs;
        private IDs_1D _localIDs;

        public _1D(int globalWorkSize, int localWorkSize = 1)
        {
            _globalIDs = new IDs_1D(globalWorkSize);
            _localIDs = new IDs_1D(localWorkSize);
        }

        
        public int GlobalID0
        {
            get
            {
                return (int)_globalIDs.x;
            }
        }

        
        public int LocalID0
        {
            get
            {
                return _localIDs.x;
            }
        }


        int Brahma.INDRangeDimension.Dimensions
        {
            get
            {
                return 1;
            }
        }

        IntPtr[] INDRangeDimension.GlobalWorkSize
        {
            get
            {
                return new[] { (IntPtr)_globalIDs.x };
            }
        }

        IntPtr[] INDRangeDimension.LocalWorkSize
        {
            get
            {
                return new[] { (IntPtr)_localIDs.x };
            }
        }
    }

    public struct _2D : INDRangeDimension
    {
        public struct IDs_2D
        {
            private int _x;
            private int _y;
            
            internal IDs_2D(int x, int y)
            {
                _x = x;
                _y = y;
            }
            
            
            public int x
            {
                get
                {
                    return _x;
                }
            }

            
            public int y
            {
                get
                {
                    return _y;
                }
            }
        }

        private IDs_2D _globalIDs;
        private IDs_2D _localIDs;

        public _2D(int globalWorkSizeX, int globalWorkSizeY,
            int localWorkSizeX = 1, int localWorkSizeY = 1)
        {
            _globalIDs = new IDs_2D(globalWorkSizeX, globalWorkSizeY);
            _localIDs = new IDs_2D(localWorkSizeX, localWorkSizeY);
        }

        
        public int GlobalID0
        {
            get
            {
                return _globalIDs.x;
            }
        }

        
        public int GlobalID1
        {
            get
            {
                return _globalIDs.y;
            }
        }

        
        public int LocalID0
        {
            get
            {
                return _localIDs.x;
            }
        }

        
        public int LocalID1
        {
            get
            {
                return _localIDs.y;
            }
        }

        int Brahma.INDRangeDimension.Dimensions
        {
            get
            {
                return 2;
            }
        }

        IntPtr[] INDRangeDimension.GlobalWorkSize
        {
            get
            {
                return new[] { (IntPtr)_globalIDs.x, (IntPtr)_globalIDs.y };
            }
        }

        IntPtr[] INDRangeDimension.LocalWorkSize
        {
            get
            {
                return new[] { (IntPtr)_localIDs.x, (IntPtr)_localIDs.y };
            }
        }
    }

    public struct _3D : INDRangeDimension
    {
        public struct IDs_3D
        {
            private int _x;
            private int _y;
            private int _z;

            internal IDs_3D(int x, int y, int z)
            {
                _x = x;
                _y = y;
                _z = z;
            }

            
            public int x
            {
                get
                {
                    return _x;
                }
            }

            
            public int y
            {
                get
                {
                    return _y;
                }
            }

            
            public int z
            {
                get
                {
                    return _z;
                }
            }
        }

        private IDs_3D _globalIDs;
        private IDs_3D _localIDs;

        public _3D(int globalSizeX, int globalSizeY, int globalSizeZ,
            int localSizeX = 1, int localSizeY = 1, int localSizeZ = 1)
        {
            _globalIDs = new IDs_3D(globalSizeX, globalSizeY, globalSizeZ);
            _localIDs = new IDs_3D(localSizeX, localSizeY, localSizeZ);
        }
        
        public int GlobalID0
        {
            get
            {
                return _globalIDs.x;
            }
        }
        
        public int GlobalID1
        {
            get
            {
                return _globalIDs.y;
            }
        }
                
        public int GlobalID2
        {
            get
            {
                return _globalIDs.z;
            }
        }
        
        public int LocalID0
        {
            get
            {
                return _localIDs.x;
            }
        }
        
        public int LocalID1
        {
            get
            {
                return _localIDs.y;
            }
        }
        
        public int LocalID2
        {
            get
            {
                return _localIDs.z;
            }
        }

        int Brahma.INDRangeDimension.Dimensions
        {
            get
            {
                return 3;
            }
        }

        IntPtr[] INDRangeDimension.GlobalWorkSize
        {
            get
            {
                return new[] { (IntPtr)_globalIDs.x, (IntPtr)_globalIDs.y, (IntPtr)_globalIDs.z };
            }
        }

        IntPtr[] INDRangeDimension.LocalWorkSize
        {
            get
            {
                return new[] { (IntPtr)_localIDs.x, (IntPtr)_localIDs.y, (IntPtr)_localIDs.z };
            }
        }
    }
}
