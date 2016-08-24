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

namespace Brahma.Commands
{
    public abstract class WaitFor: Command
    {
        private const string AnonymousWaitForName = "WaitFor_{0}";
        private static int _anonymousWaitForID;

        protected WaitFor()
        {
            Name = string.Format(AnonymousWaitForName, _anonymousWaitForID++);
        }

        public static Command operator &(WaitFor wait, Command command)
        {
            command.WaitList.AddRange(wait.WaitList);
            return command;
        }

        public static WaitFor operator &(WaitFor wait1, WaitFor wait2)
        {
            wait1.WaitList.AddRange(wait2.WaitList);
            return wait1;
        }
    }
}