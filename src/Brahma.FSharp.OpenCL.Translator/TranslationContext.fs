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

namespace Brahma.FSharp.OpenCL.Translator

open System.Collections.Generic
open Brahma.FSharp.OpenCL.AST
open System

type ArrayKind =
    | CPointer
    | CArrayDecl of size: int

type Flag =
    | EnableAtomic
    | EnableFP64

type Namer() =
    let mutable counter = 0
    let scopes = Stack<Dictionary<string, string>> 10
    let allVars = ResizeArray<string>()
    let forAdd = Dictionary<string, string>()

    let newName vName =
        if allVars.Contains vName then
            let n = vName + string counter
            counter <- counter + 1
            n
        else
            allVars.Add vName
            vName

    member this.LetIn() = scopes.Push <| Dictionary()

    member this.LetStart bindingName =
        let newName = newName bindingName
        //forAdd.Add(bindingName,newName)
        forAdd.[bindingName] <- newName
        newName

    member this.LetIn bindingName =
        scopes.Push <| Dictionary()
        this.AddVar bindingName

    member this.LetOut() = scopes.Pop() |> ignore

    member this.AddVar vName =
        let newName =
            if forAdd.ContainsKey vName then
                let n = forAdd.[vName]
                forAdd.Remove vName |> ignore
                n
            else
                newName vName

        scopes.Peek().Add(vName, newName)

    member this.GetCLVarName vName =
        scopes.ToArray()
        |> Array.tryFind (fun scope -> scope.ContainsKey vName)
        |> Option.map (fun scope -> scope.[vName])

    member this.GetUnicName name = newName name

type TranslationContext<'lang, 'vDecl> =
    {
        // translator scope
        TranslatorOptions: TranslatorOptions

        // kernel scope
        CStructDecls: Dictionary<Type, StructType<'lang>>
        StructInplaceCounter: Dictionary<string, int>
        TopLevelVarsDecls: ResizeArray<'vDecl>
        Flags: HashSet<Flag>

        // function scope
        VarDecls: ResizeArray<'vDecl>
        Namer: Namer

        // specific scope
        ArrayKind: ArrayKind
    }

    static member Create(options) =
        {
            TranslatorOptions = options

            CStructDecls = Dictionary<Type, StructType<'lang>>()
            StructInplaceCounter = Dictionary<string, int>()
            TopLevelVarsDecls = ResizeArray<'vDecl>()
            Flags = HashSet()

            VarDecls = ResizeArray<'vDecl>()
            Namer = Namer()

            ArrayKind = CPointer
        }

    member this.WithNewLocalContext() = { this with VarDecls = ResizeArray(); Namer = Namer() }

type TargetContext = TranslationContext<Lang, Statement<Lang>>

[<AutoOpen>]
module Translation =
    let translation = StateBuilder<TargetContext>()
