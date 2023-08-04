// Copyright (c) 2012, 2013 Semyon Grigorev <rsdpisuy@gmail.com>
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

type AST<'lang>(topDefs: List<ITopDef<'lang>>) =
    member this.TopDefs = topDefs

//type TopLevelVarDecl<'lang>(name: string, _type:Type<'lang>, spaceModifier:AddressSpaceQualifier<'lang>, ?value: obj) =
//    inherit TopDef<'lang>()
//    override this.Children = []
//    member this.Name = name
//    member this.Type = _type
//    member this.SpaceModifier = spaceModifier
//    member this.Value = value
