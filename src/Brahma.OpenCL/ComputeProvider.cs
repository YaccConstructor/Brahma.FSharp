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
using System.Linq.Expressions;
using System.Text;
using System.Text.RegularExpressions;

using OpenCL.Net.Extensions;
using OpenCL.Net;

namespace Brahma.OpenCL
{
    [Flags]
    public enum CompileOptions
    {
        UseNativeFunctions = 1 << 0,
        FastRelaxedMath = 1 << 1,
        FusedMultiplyAdd = 1 << 2,
        DisableOptimizations = 1 << 3,
        StrictAliasing = 1 << 4,
        NoSignedZeros = 1 << 5,
        UnsafeMathOptimizations = 1 << 6,
        FiniteMathOnly = 1 << 7
    }
    
    public sealed class ComputeProvider: Brahma.ComputeProvider
    {        
        private const CompileOptions DefaultOptions = CompileOptions.UseNativeFunctions | CompileOptions.FusedMultiplyAdd | CompileOptions.FastRelaxedMath;
        
        private readonly Context _context;
        private readonly Device[] _devices;
        private bool _disposed;
        private string _compileOptions = string.Empty;

        private Dictionary<object, Mem> _autoconfiguredBuffers = new Dictionary<object, Mem>(5);

        public void CloseAllBuffers()
        {
            foreach (var kvp in this._autoconfiguredBuffers)
            {
                kvp.Value.Dispose();
            }
            _autoconfiguredBuffers.Clear();
        }

        public Dictionary<object, Mem> AutoconfiguredBuffers
        {
            get { return _autoconfiguredBuffers; }
        }

        public String CompileOptionsStr
        {
            get
            {
                return _compileOptions;
            }
        }

        public static CompileOptions DefaultOptions_p
        {
            get
            {
                return DefaultOptions;
            }
        }

        public void SetCompileOptions(CompileOptions options)
        {
            CompileOptions = options;
            
            _compileOptions = string.Empty;

            // UseNativeFunctions = ((options & CompileOptions.UseNativeFunctions) == CompileOptions.UseNativeFunctions);
            _compileOptions += ((options & CompileOptions.FastRelaxedMath) == CompileOptions.FastRelaxedMath ? " -cl-fast-relaxed-math " : string.Empty);
            _compileOptions += ((options & CompileOptions.FusedMultiplyAdd) == CompileOptions.FusedMultiplyAdd ? " -cl-mad-enable " : string.Empty);
            _compileOptions += ((options & CompileOptions.DisableOptimizations) == CompileOptions.DisableOptimizations ? " -cl-opt-disable " : string.Empty);
            _compileOptions += ((options & CompileOptions.StrictAliasing) == CompileOptions.StrictAliasing ? " -cl-strict-aliasing " : string.Empty);
            _compileOptions += ((options & CompileOptions.NoSignedZeros) == CompileOptions.NoSignedZeros ? " -cl-no-signed-zeros " : string.Empty);
            _compileOptions += ((options & CompileOptions.UnsafeMathOptimizations) == CompileOptions.UnsafeMathOptimizations ? " -cl-unsafe-math-optimizations " : string.Empty);
            _compileOptions += ((options & CompileOptions.FiniteMathOnly) == CompileOptions.FiniteMathOnly ? " -cl-finite-math-only " : string.Empty);
        }

        internal CompileOptions CompileOptions
        {
            get;
            private set;
        }

        public Context Context
        {
            get 
            {
                return _context;
            }
        }
        
        public ComputeProvider(params Device[] devices)
        {                
            if (devices == null)
                throw new ArgumentNullException("devices");
            if (devices.Length == 0)
                throw new ArgumentException("Need at least one device!");
            
            _devices = devices;
            
            ErrorCode error;
            _context = Cl.CreateContext(null, (uint)devices.Length, _devices, null, IntPtr.Zero, out error);
            
            if (error != ErrorCode.Success)
                throw new Cl.Exception(error);
        }        
       

        public override void Dispose()
        {
            if (!_disposed)
            {
                _context.Release();                
                _context.Dispose();
                _disposed = true;
            }
        }

        public override string ToString()
        {
            var builder = new StringBuilder();
            foreach (var device in _devices)
            {
                ErrorCode error;
                var platform =
                    Cl.GetDeviceInfo(device, DeviceInfo.Platform, out error).CastTo<Platform>();
                var deviceType =
                    Cl.GetDeviceInfo(device, DeviceInfo.Type, out error).CastTo<DeviceType>();

                builder.AppendFormat("[Platform: {0}, device type:{1}]\n",
                                     Cl.GetPlatformInfo(platform, PlatformInfo.Name, out error), deviceType);
            }

            return builder.ToString();
        }

        public IEnumerable<Device> Devices
        {
            get
            {
                return _devices;
            }
        }

        private static string WildcardToRegex(string pattern)
        {
            return "^" + Regex.Escape(pattern).
            Replace("\\*", ".*").
            Replace("\\?", ".") + "$";
        }

        public static ComputeProvider Create(string platformName = "*", DeviceType deviceType = DeviceType.Default)
        {
            var platformNameRegex = new Regex(WildcardToRegex(platformName), RegexOptions.IgnoreCase);
            Platform? currentPlatform = null;
            ErrorCode error;
            foreach (var platform in Cl.GetPlatformIDs(out error))
                if (platformNameRegex.Match(Cl.GetPlatformInfo(platform, PlatformInfo.Name, out error).ToString()).Success)
                {
                    currentPlatform = platform;
                    break;
                }

            if (currentPlatform == null)
                throw new PlatformNotSupportedException(string.Format("Could not find a platform that matches {0}", platformName));

            var compatibleDevices = from device in Cl.GetDeviceIDs(currentPlatform.Value, deviceType, out error)
                                    select device;
            if (compatibleDevices.Count() == 0)
                throw new PlatformNotSupportedException(string.Format("Could not find a device with type {0} on platform {1}",
                    deviceType, Cl.GetPlatformInfo(currentPlatform.Value, PlatformInfo.Name, out error)));

            return new ComputeProvider(compatibleDevices.ToArray());
        }
    }
}