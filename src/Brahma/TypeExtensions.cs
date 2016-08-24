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

namespace Brahma
{
    public static class TypeExtensions
    {
        public static bool IsAnonymous(this Type type)
        {
            return type.Name.StartsWith("<>f__AnonymousType");
        }

        // TODO: Change all calls that use IsAssignableFrom to use DerivesFrom
        public static bool DerivesFrom(this Type type, Type baseType)
        {
            return type.IsSubclassOf(baseType);
        }

        public static bool Implements(this Type type, Type interfaceType)
        {
            if (!interfaceType.IsInterface)
                throw new ArgumentException(string.Format("{0} is not an interface", interfaceType.FullName));
            return type.GetInterface(interfaceType.FullName) != null;
        }

        public static bool IsEnumerable(this Type type)
        {
            return type.GetInterface(typeof(IEnumerable).FullName) != null;
        }

        public static Type GetEnumeratorType(this Type type)
        {
            if (type.IsEnumerable())
                return type.GetGenericArguments()[0];
            
            return null;
        }

        public static bool IsConcreteGenericOf(this Type type, Type openGeneric)
        {
            if (!type.IsGenericType)
                return false;
            if (!openGeneric.IsGenericType)
                return false;
            if (!openGeneric.IsGenericTypeDefinition)
                return false;

            return type.GetGenericTypeDefinition() == openGeneric;
        }

        public static bool IsAssignableTo(this Type type, Type baseType)
        {
            return baseType.IsAssignableFrom(type);
        }
    }
}