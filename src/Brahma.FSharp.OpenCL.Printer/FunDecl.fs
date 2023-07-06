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
open Brahma.FSharp.OpenCL.Printer

module FunDecl =
    let private printFunFormalParam (param: FunFormalArg<_>) =
        [ match param.DeclSpecs.AddressSpaceQualifier with
          | Global -> yield wordL "__global"
          | Local -> yield wordL "__local"
          | _ -> yield wordL "private"

          match param.DeclSpecs.Type with
          | Some t -> yield Types.print t
          | None -> failwith "Could not print a formal arg with undefined type"

          yield wordL param.Name ]
        |> spaceListL

    let print<'lang> (funDecl: FunDecl<'lang>) =
        let isVoidArg (arg: FunFormalArg<_>) =
            match arg.DeclSpecs.Type with
            | Some x ->
                match x with
                | :? PrimitiveType<_> as p -> p.Type = Void
                | _ -> false
            | None -> false

        let header =
            [ match funDecl.DeclSpecs.FunQual with
              | Some Kernel -> yield wordL "__kernel"
              | None -> ()
              match funDecl.DeclSpecs.Type with
              | Some t -> yield Types.print t
              | None -> failwith "Could not print a func declaration with undefined return type"
              yield wordL funDecl.Name ]
            |> spaceListL

        let formalParams =
            funDecl.Args
            |> List.filter (not << isVoidArg)
            |> List.map printFunFormalParam
            |> commaListL
            |> bracketL

        let body = Statements.print true funDecl.Body
        aboveL (header ++ formalParams) body
