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

open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.AST
open Brahma.FSharp.OpenCL.Translator.QuotationTransformers
open Brahma.FSharp

type FSQuotationToOpenCLTranslator(device: IDevice, ?translatorOptions: TranslatorOptions) =
    let translatorOptions = defaultArg translatorOptions (TranslatorOptions())
    let mainKernelName = "brahmaKernel"
    let lockObject = obj ()

    let constructMethods (expr: Expr) (functions: (Var * Expr) list) =
        let kernelFunc =
            KernelFunc(Var(mainKernelName, expr.Type), expr) :> Method |> List.singleton

        let methods =
            functions
            |> List.map (fun (var, expr) -> Function(var, expr) :> Method)

        methods @ kernelFunc

    let transformQuotation expr =
        expr
        |> Print.replace
        |> WorkSize.get
        |> Atomic.parse
        |> Names.makeUnique
        |> Variables.defsToLambda
        |> VarToRef.transform
        |> Names.makeUnique
        |> Lift.parse

    let createTranslationContext (kernelExpr: Expr) (methods: Method list) =
        let context = TranslationContext.Create(translatorOptions)
        let clFuns = ResizeArray()

        let kernelArgumentsNames =
            kernelExpr
            |> Utils.collectLambdaArguments
            |> List.map (fun var -> var.Name)

        let localVarsNames =
            kernelExpr
            |> Utils.getLocalVars
            |> List.map (fun var -> var.Name)

        methods
        |> List.iter (fun method -> clFuns.AddRange(method.Translate(kernelArgumentsNames, localVarsNames) |> State.eval context))

        context, clFuns

    let getPragmas (context: TranslationContext<_, _>) =
        let pragmas = ResizeArray()

        context.Flags
        |> Seq.iter (function
            | EnableAtomic ->
                pragmas.Add(CLPragma CLGlobalInt32BaseAtomics :> ITopDef<_>)
                pragmas.Add(CLPragma CLLocalInt32BaseAtomics :> ITopDef<_>)
            | EnableFP64 -> pragmas.Add(CLPragma CLFP64))

        List.ofSeq pragmas

    let getUserDefinedTypes (context: TranslationContext<_, _>) =
        context.CStructDecls.Values
        |> Seq.map StructDecl
        |> Seq.cast<ITopDef<Lang>>
        |> List.ofSeq

    let translate expr =
        let kernelExpr, functions = transformQuotation expr

        let kernelFun = KernelFunc(Var(mainKernelName, kernelExpr.Type), kernelExpr)
        let methodsFun = functions |> List.map (fun (var, expr) -> Function(var, expr) :> Method)

        let methods = (kernelFun :> Method |> List.singleton) @ methodsFun

        let context, clFuns = createTranslationContext kernelExpr methods

        let pragmas = getPragmas context

        let userDefinedTypes = getUserDefinedTypes context

        let ast = AST(pragmas @ userDefinedTypes @ List.ofSeq clFuns)

        ast, kernelFun.FunExpr

    member val Marshaller = CustomMarshaller()

    member this.TranslatorOptions = translatorOptions

    member this.Translate(qExpr) =
        lock lockObject <| fun () -> translate qExpr

    member this.TransformQuotation(expr: Expr) = transformQuotation expr

    static member CreateDefault() =
        let device =
            { new IDevice with
                member this.Name = ""
                member this.Platform = Platform.Any
                member this.DeviceType = DeviceType.Default
                member this.MaxWorkGroupSize = 0
                member this.MaxWorkItemDimensions = 0
                member this.MaxWorkItemSizes = [| 0 |]
                member this.DeviceExtensions = [||]
                member this.LocalMemSize = 0<Byte>
                member this.GlobalMemSize = 0L<Byte>
            }

        FSQuotationToOpenCLTranslator(device)
