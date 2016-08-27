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
using ClNet = OpenCL.Net;
using Cl = OpenCL.Net.Cl; 

namespace Brahma.OpenCL
{
    public sealed class CommandQueue: Brahma.CommandQueue
    {
        private static readonly Dictionary<string, ClNet.Event> _namedEvents =
            new Dictionary<string, ClNet.Event>();
        
        private bool _disposed = false;
        private ClNet.CommandQueue _queue;

        internal ClNet.CommandQueue Queue
        {
            get
            {
                return _queue;
            }
            set { _queue = value; }
        }

        public static void Cleanup()
        {
            ClNet.ErrorCode error;
            foreach (var name in (from kvp in _namedEvents
                                  let name = kvp.Key
                                  let ev = kvp.Value
                                  let status = ClNet.Cl.GetEventInfo(ev, ClNet.EventInfo.CommandExecutionStatus, out error).CastTo<ClNet.ExecutionStatus>()
                                  where status == ClNet.ExecutionStatus.Complete
                                  select name))
            {
                _namedEvents[name].Dispose();
                _namedEvents.Remove(name);
            }
        }

        internal static void AddEvent(string name, ClNet.Event ev)
        {
            _namedEvents.Add(name, ev);
        }

        internal static ClNet.Event? FindEvent(string eventName)
        {
            if (_namedEvents.ContainsKey(eventName))
                return _namedEvents[eventName];

            return null;
        }

        public CommandQueue(ComputeProvider provider, ClNet.Device device, bool outOfOrderExecution = false)
        {
            ClNet.ErrorCode error;
            _queue = Cl.CreateCommandQueue
                (provider.Context
                , device
                , outOfOrderExecution 
                    ? ClNet.CommandQueueProperties.OutOfOrderExecModeEnable
                    : ClNet.CommandQueueProperties.None, out error);

            if (error != ClNet.ErrorCode.Success)
                throw new Cl.Exception(error);
        }

        public override Brahma.CommandQueue Add(params Command[] commands)
        {
            foreach (var command in commands)
                command.Execute(this);

            return this;
        }

        public override Brahma.CommandQueue Finish()
        {
            ClNet.ErrorCode error = Cl.Finish(_queue);

            if (error != ClNet.ErrorCode.Success)
                throw new Cl.Exception(error);

            return this;
        }

        public override Brahma.CommandQueue Barrier()
        {
            ClNet.ErrorCode error = Cl.EnqueueBarrier(_queue);

            if (error != ClNet.ErrorCode.Success)
                throw new Cl.Exception(error);

            return this;
        }

        public override void Dispose()
        {
            if (!_disposed)
            {
                _queue.Dispose();
                //Queue = null;
                _disposed = true;
            }
        }
    }
}