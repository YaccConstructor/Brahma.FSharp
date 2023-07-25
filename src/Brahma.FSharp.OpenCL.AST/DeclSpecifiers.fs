// Copyright (c) 2017 Kirill Smirenko <k.smirenko@gmail.com>
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

namespace Brahma.FSharp.OpenCL.AST

type FunQualifier<'lang> = | Kernel

type AddressSpaceQualifier<'lang> =
    | Global
    | Local
    | Constant
    | Private
    | Default

type AccessQualifier<'lang> =
    | ReadOnly
    | WriteOnly
    | ReadWrite

type StorageClassSpecifier<'lang> =
    | Extern
    | Static

[<RequireQualifiedAccess>]
type TypeQualifier<'lang> =
    | Const
    | Restrict
    | Volatile
