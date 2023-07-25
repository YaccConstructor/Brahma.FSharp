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

type DeclSpecifierPack<'lang>
    (
        ?funQualifier: FunQualifier<'lang>,
        ?addressSpaceQualifier: AddressSpaceQualifier<'lang>,
        ?accessQualifier: AccessQualifier<'lang>,
        ?storageClassSpecifier: StorageClassSpecifier<'lang>,
        ?typeSpecifier: Type<'lang>,
        ?typeQualifiers: TypeQualifier<'lang> list
    ) =

    inherit Node<'lang>()

    override this.Children = []

    member val FunQual = funQualifier with get, set
    member val AddressSpaceQualifier = defaultArg addressSpaceQualifier Default with get, set
    member val AccessQual = accessQualifier with get, set
    member val StorageClassSpec = storageClassSpecifier with get, set
    member val Type = typeSpecifier with get, set
    member val TypeQualifiers = defaultArg typeQualifiers [] with get, set

    member this.AddTypeQual tq =
        this.TypeQualifiers <- tq :: this.TypeQualifiers

    member this.Matches(other: obj) =
        match other with
        | :? DeclSpecifierPack<'lang> as other ->
            let areTypesMatching =
                match this.Type, other.Type with
                | Some x, Some y -> x.Matches(y)
                | None, None -> true
                | _ -> false

            this.FunQual = other.FunQual
            && this.AddressSpaceQualifier = other.AddressSpaceQualifier
            && this.AccessQual = other.AccessQual
            && this.StorageClassSpec = other.StorageClassSpec
            && areTypesMatching
            && this.TypeQualifiers = other.TypeQualifiers
        | _ -> false
