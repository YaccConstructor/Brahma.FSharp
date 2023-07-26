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
open Microsoft.FSharp.Text.StructuredFormat
open Brahma.FSharp.OpenCL.Printer

module AST =
    let print (ast: AST<'lang>) =
        ast.TopDefs
        |> List.map (fun d ->
            match d with
            | :? FunDecl<'lang> as fd -> FunDecl.print fd
            | :? CLPragma<'lang> as clp -> Pragmas.print clp
            | :? StructDecl<'lang> as s -> TypeDecl.printStructDeclaration s
            | :? VarDecl<'lang> as s -> Statements.print false s
            | _ -> failwithf "Printer. Unsupported toplevel declaration: %A" d
        )
        // |> LayoutOps.sepListL (LayoutOps.wordL "\r\n")
        // |> Display.layout_to_string FormatOptions.Default
        |> LayoutOps.aboveListL
        |> Display.layout_to_string { FormatOptions.Default with PrintWidth = 100 }
