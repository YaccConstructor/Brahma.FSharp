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

namespace Brahma.FSharp.OpenCL.Printer

open Brahma.FSharp.OpenCL.AST
open Microsoft.FSharp.Text.StructuredFormat.LayoutOps

module Types =
    let private printPrimitiveType (pType: PrimitiveType<'lang>) =
        match pType.Type with
        | Bool -> "bool"
        | Char -> "char"
        | UChar -> "uchar"
        | Short -> "short"
        | UShort -> "ushort"
        | Int -> "int"
        | UInt -> "uint"
        | Long -> "long"
        | ULong -> "ulong"
        | Float -> "float"
        | Double -> "double"
        | Half -> "half"
        | Void -> "void"
        | ConstStringLiteral -> "const char *"
        | TypeName tname -> tname
        |> wordL

    let rec print<'lang> (type': Type<'lang>) =
        match type' with
        | :? PrimitiveType<'lang> as pt -> printPrimitiveType pt
        | :? RefType<'lang> as rt -> print rt.BaseType ^^ wordL "*"
        | :? ArrayType<'lang> as art -> print art.BaseType
        | :? Image2DType<'lang> as imgt ->
            match imgt.Modifier with
            | true -> wordL "read_only image2D"
            | false -> wordL "write_only image2D"
        | :? StructInplaceType<'lang> as s -> printStructInplaceType s
        | :? StructType<'lang> as s -> wordL s.Name
        | :? UnionClInplaceType<'lang> as u -> printUnionInplaceType u
        | :? TupleType<'lang> as t -> wordL t.BaseStruct.Name
        | _ -> failwithf "Printer. Unsupported type: %A" type'

    and printAggregatingInplaceType keyword typeName fields =
        let header = [ wordL keyword; wordL typeName ] |> spaceListL

        let body =
            [
                for field in fields -> [ print field.Type; wordL field.Name; wordL ";" ] |> spaceListL
            ]
            |> aboveListL
            |> braceL

        header ^^ body

    and printUnionInplaceType (t: UnionClInplaceType<_>) =
        printAggregatingInplaceType "union" t.Name t.Fields

    and printStructInplaceType (t: StructInplaceType<_>) =
        printAggregatingInplaceType "struct" t.Name t.Fields
