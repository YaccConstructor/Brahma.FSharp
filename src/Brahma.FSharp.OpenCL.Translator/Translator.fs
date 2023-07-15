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
open System.Collections.Generic
open Brahma.FSharp

type FSQuotationToOpenCLTranslator(device: IDevice, ?translatorOptions: TranslatorOptions) =
    let translatorOptions = defaultArg translatorOptions (TranslatorOptions())
    let mainKernelName = "brahmaKernel"
    let lockObject = obj ()

    let collectData (expr: Expr) (functions: (Var * Expr) list) =
        // global var names
        let kernelArgumentsNames =
            expr |> Utils.collectLambdaArguments |> List.map (fun var -> var.Name)

        let localVarsNames =
            expr |> Utils.collectLocalVars |> List.map (fun var -> var.Name)

        let atomicApplicationsInfo =
            let atomicPointerArgQualifiers = Dictionary<Var, AddressSpaceQualifier<Lang>>()

            let (|AtomicApplArgs|_|) (args: Expr list list) =
                match args with
                | [ mutex ] :: _ :: [ [ DerivedPatterns.SpecificCall <@ ref @> (_, _, [ Patterns.ValidVolatileArg var ]) ] ]
                | [ mutex ] :: [ [ DerivedPatterns.SpecificCall <@ ref @> (_, _, [ Patterns.ValidVolatileArg var ]) ] ] ->
                    Some(mutex, var)
                | _ -> None

            let rec go expr =
                match expr with
                | DerivedPatterns.Applications(Patterns.Var funcVar, AtomicApplArgs(_, volatileVar)) when
                    funcVar.Name.StartsWith "atomic"
                    ->

                    if kernelArgumentsNames |> List.contains volatileVar.Name then
                        atomicPointerArgQualifiers.Add(funcVar, Global)
                    elif localVarsNames |> List.contains volatileVar.Name then
                        atomicPointerArgQualifiers.Add(funcVar, Local)
                    else
                        failwith "Atomic pointer argument should be from local or global memory only"

                | ExprShape.ShapeVar _ -> ()
                | ExprShape.ShapeLambda(_, lambda) -> go lambda
                | ExprShape.ShapeCombination(_, exprs) -> List.iter go exprs

            functions |> List.map snd |> (fun tail -> expr :: tail) |> Seq.iter go

            atomicPointerArgQualifiers |> Seq.map (|KeyValue|) |> Map.ofSeq

        kernelArgumentsNames, localVarsNames, atomicApplicationsInfo

    let constructMethods
        (expr: Expr)
        (functions: (Var * Expr) list)
        (atomicApplicationsInfo: Map<Var, AddressSpaceQualifier<Lang>>)
        =
        let kernelFunc =
            KernelFunc(Var(mainKernelName, expr.Type), expr) :> Method |> List.singleton

        let methods =
            functions
            |> List.map (fun (var, expr) ->
                match atomicApplicationsInfo |> Map.tryFind var with
                | Some qual -> AtomicFunc(var, expr, qual) :> Method
                | None -> Function(var, expr) :> Method)

        methods @ kernelFunc

    // TODO(add logging (via CE state))

    let transformQuotation expr =
        expr
        |> Print.replace
        |> WorkSize.get
        |> Atomic.parse
        |> makeVarNameUnique
        |> transformVarDefsToLambda
        |> transformMutableVarsToRef
        |> makeVarNameUnique
        |> lambdaLifting

    let translate expr =
        // TODO: Extract quotationTransformer to translator
        // what is it?
        let kernelExpr, functions = transformQuotation expr

        let globalVars, localVars, atomicApplicationsInfo = collectData kernelExpr functions

        let methods = constructMethods kernelExpr functions atomicApplicationsInfo

        let context = TranslationContext.Create(translatorOptions)
        let clFuncs = ResizeArray()

        methods
        |> List.iter (fun method -> clFuncs.AddRange(method.Translate(globalVars, localVars) |> State.eval context))

        let pragmas =
            let pragmas = ResizeArray()

            context.Flags
            |> Seq.iter (fun (flag: Flag) ->
                match flag with
                | EnableAtomic ->
                    pragmas.Add(CLPragma CLGlobalInt32BaseAtomics :> ITopDef<_>)
                    pragmas.Add(CLPragma CLLocalInt32BaseAtomics :> ITopDef<_>)
                | EnableFP64 -> pragmas.Add(CLPragma CLFP64))

            List.ofSeq pragmas

        let userDefinedTypes =
            context.CStructDecls.Values
            |> Seq.map StructDecl
            |> Seq.cast<ITopDef<Lang>>
            |> List.ofSeq

        AST(pragmas @ userDefinedTypes @ List.ofSeq clFuncs),
        methods
        |> List.find (fun method -> method :? KernelFunc)
        |> fun kernel -> kernel.FunExpr

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
                member this.GlobalMemSize = 0L<Byte> }

        FSQuotationToOpenCLTranslator(device)
