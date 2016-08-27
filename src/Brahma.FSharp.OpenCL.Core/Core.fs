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

module Brahma.FSharp.OpenCL.Core

open Brahma.OpenCL
open Microsoft.FSharp.Quotations
open FSharp.Quotations.Evaluator
open Brahma.FSharp.OpenCL
open OpenCL.Net
open System

type CLCodeGenerator() =
    static member KernelName = "brahmaKernel"
    static member GenerateKernel(lambda: Expr, provider: ComputeProvider, kernel:ICLKernel, translatorOptions) =        
        let codeGenerator = new Translator.FSQuotationToOpenCLTranslator()
        let ast, newLambda = codeGenerator.Translate lambda translatorOptions
        let code = Printer.AST.Print ast 
        kernel.Provider <- provider        
        kernel.Source <- kernel.Source.Append code
        kernel.SetClosures [||]
        kernel.SetParameters []   
        newLambda     
        
type ComputeProvider with

    member private this.CompileQuery<'T when 'T :> ICLKernel>(lambda:Expr, translatorOptions) =
        let kernel = System.Activator.CreateInstance<'T>()        
//        let r, newLambda = CLCodeGenerator.GenerateKernel(lambda, this, kernel, translatorOptions)
        let r = CLCodeGenerator.GenerateKernel(lambda, this, kernel, translatorOptions)
        let str = (kernel :> ICLKernel).Source.ToString()    
        let program, error = Cl.CreateProgramWithSource(this.Context, 1u, [|str|], null)
        let _devices = Array.ofSeq  this.Devices
        let error = Cl.BuildProgram(program, _devices.Length |> uint32, _devices, this.CompileOptionsStr, null, IntPtr.Zero)
        if error <> ErrorCode.Success
        then 
            let s = _devices |> Array.map (fun device -> Cl.GetProgramBuildInfo(program, device, ProgramBuildInfo.Log ) |> fst |> string)
            s
            |> String.concat "\n"
            |> failwith
        let clKernel,errpr = Cl.CreateKernel(program, CLCodeGenerator.KernelName)
        (kernel :> ICLKernel).ClKernel <- clKernel
            
//        kernel , newLambda 
        kernel            
                    
    member this.Compile (query: Expr<'TRange ->'a> , ?_options:CompileOptions, ?translatorOptions, ?_outCode:string ref, ?kernelName:string) =
        let options = defaultArg _options ComputeProvider.DefaultOptions_p
        let tOptions = defaultArg translatorOptions []
        this.SetCompileOptions options
//        let kernel, newQuery = this.CompileQuery<Kernel<'TRange>>(query, tOptions)
        let kernel = this.CompileQuery<Kernel<'TRange>>(query, tOptions)
        let rng = ref Unchecked.defaultof<'TRange>
        let args = ref [||]
        let run = ref Unchecked.defaultof<Commands.Run<'TRange>>
        let getStarterFuncton qExpr =
            let rec go expr vars =
                match expr with
                | Patterns.Lambda (v, body) ->
                    Expr.Lambda(v, go body (v::vars))
                | e ->
                    let arr =
                        let c = Expr.NewArray(typeof<obj>,vars |> List.rev 
                            |> List.map 
                                 (fun v -> Expr.Coerce (Expr.Var(v), typeof<obj>)))
                        <@@
                            let x = %%c |> List.ofArray
                            rng := (box x.Head) :?> 'TRange
                            args := x.Tail |> Array.ofList
                            let brahmsRunCls = new Brahma.OpenCL.Commands.Run<_>(kernel,!rng)
                            !args |> Array.iteri (fun i x -> brahmsRunCls.SetupArgument(1,i,x))
                            run := kernel.Run(!rng, !args)
                        @@>
                    arr
            let res = <@ %%(go qExpr []):'TRange ->'a @>.Compile()
            
            res
                    
        if _outCode.IsSome then (_outCode.Value) := (kernel :> ICLKernel).Source.ToString()

        kernel
        , getStarterFuncton query
        , fun () -> !run
    