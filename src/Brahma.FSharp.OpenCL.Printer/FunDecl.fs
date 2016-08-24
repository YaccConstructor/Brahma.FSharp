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

module Brahma.FSharp.OpenCL.Printer.FunDecl

open Brahma.FSharp.OpenCL.AST
open Microsoft.FSharp.Text.StructuredFormat
open Microsoft.FSharp.Text.StructuredFormat.LayoutOps
open Brahma.FSharp.OpenCL.Printer

let private printFunFormalParam (param:FunFormalArg<_>) =
    [ if param.IsGlobal 
      then if param.Name.[0] <> '_'
           then yield wordL "__global"
           else yield wordL "__global"
      else yield wordL "private"
    ; yield Types.Print param.Type
    ; yield wordL param.Name
    ] |> spaceListL
    

let Print<'lang> (funDecl:FunDecl<'lang>) =
    let header = 
        [ if funDecl.isKernel then yield wordL "__kernel"
        ; yield Types.Print funDecl.RetType
        ; yield wordL funDecl.Name
        ] |> spaceListL
    let formalParams = funDecl.Args |> List.map printFunFormalParam |> commaListL |> bracketL
    let body = Statements.Print true funDecl.Body 
    aboveL (header ++ formalParams) body
