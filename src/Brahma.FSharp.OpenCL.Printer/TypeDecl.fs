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

module Brahma.FSharp.OpenCL.Printer.TypeDecl

open Brahma.FSharp.OpenCL.AST
open Microsoft.FSharp.Text.StructuredFormat
open Microsoft.FSharp.Text.StructuredFormat.LayoutOps

let PrintStructDeclaration (decl:Struct<_>) =
    let header = 
        [
            wordL "typedef"
            wordL "struct "
            wordL decl.Name

        ]
        |> spaceListL
       
    let flds = 
        [ 
            for f in decl.Fields ->
                [
                    Types.Print f.FType
                    wordL f.FName
                    wordL ";"
                ] 
                |> spaceListL
        ]
        |> aboveListL
        |> braceL
    let footer = 
        [            
            wordL decl.Name
            wordL ";"
        ]
        |> spaceListL
    header ^^ flds ^^ footer

