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

module Brahma.FSharp.OpenCL.Printer.AST

open Brahma.FSharp.OpenCL.AST
open Microsoft.FSharp.Text
open Microsoft.FSharp.Text.StructuredFormat.LayoutOps
open Brahma.FSharp.OpenCL.Printer

let Print (ast:AST<'lang>) =
    let layout = 
        ast.TopDefs 
        |> List.map 
            (fun d -> 
                match d with 
                | :? FunDecl<'lang> as fd -> FunDecl.Print fd
                | :? CLPragma<'lang> as clp -> Pragmas.Print clp
                | :? Struct<'lang> as s -> TypeDecl.PrintStructDeclaration s
                | _ -> failwithf "Printer. Unsupported toplevel declaration: %A"  d)
        |> aboveListL
    let result = StructuredFormat.Display.layout_to_string {StructuredFormat.FormatOptions.Default with PrintWidth=100} layout
    //printfn "%A" result
    result
