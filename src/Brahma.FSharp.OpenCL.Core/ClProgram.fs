namespace Brahma.FSharp

open Brahma.FSharp
open Brahma.FSharp.OpenCL
open OpenCL.Net
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Printer
open System
open Brahma.FSharp.OpenCL.Translator
open Brahma.FSharp.OpenCL.Shared
open Brahma.FSharp.OpenCL.Translator.QuotationTransformers
open System.Runtime.InteropServices
open FSharp.Quotations.Evaluator

/// Represents an abstraction over OpenCL program.
type ClProgram<'TRange, 'a when 'TRange :> INDRange>(ctx: ClContext, srcLambda: Expr<'TRange -> 'a>) =

    let compilerOptions =
        defaultArg ctx.CompilerOptions " -cl-fast-relaxed-math -cl-mad-enable -cl-unsafe-math-optimizations "

    let (clCode, newLambda) =
        let (ast, newLambda) = ctx.Translator.Translate(srcLambda)
        let code = AST.print ast
        code, newLambda

    let program =
        let (program, error) =
            let sources = [| clCode |]
            Cl.CreateProgramWithSource(ctx.Context, uint32 sources.Length, sources, null)

        if error <> ErrorCode.Success then
            failwithf $"Program creation failed: %A{error}"

        let error =
            Cl.BuildProgram(program, 1u, [| ctx.ClDevice.Device |], compilerOptions, null, IntPtr.Zero)

        if error <> ErrorCode.Success then
            let errorCode = ref ErrorCode.Success

            let buildInfo =
                Cl.GetProgramBuildInfo(program, ctx.ClDevice.Device, ProgramBuildInfo.Log, errorCode)

            failwithf $"Program compilation failed: %A{error} \n   BUILD LOG:\n %A{buildInfo} \n"

        program

    let setupArgument (kernel: Kernel) index (arg: obj) =
        let toIMem arg =
            match box arg with
            | :? IClMem as buf -> buf.Size, buf.Data
            | :? int as i -> IntPtr(Marshal.SizeOf i), box i
            | other -> failwithf $"Unexpected argument: %A{other}"

        let (argSize, argVal) = toIMem arg
        let error = Cl.SetKernelArg(kernel, uint32 index, argSize, argVal)

        if error <> ErrorCode.Success then
            raise (CLException error)

    let kernelPrepare =
        match newLambda with
        | DerivedPatterns.Lambdas(lambdaArgs, _) ->
            let args = List.concat lambdaArgs

            let regularArgs =
                Expr.NewArray(typeof<obj>, args |> List.map (fun v -> Expr.Coerce(Expr.Var v, typeof<obj>)))

            let argsList = args |> List.map List.singleton

            let kernelVar = Var("kernel", typeof<IKernel>)
            let rangeVar = Var("range", typeof<'TRange ref>)
            let argsVar = Var("args", typeof<obj[] ref>)

            let xVar = Var("x", typeof<obj list>)

            Expr.Lambdas(
                [ [ kernelVar ]; [ rangeVar ]; [ argsVar ] ] @ argsList,
                Expr.Let(
                    xVar,
                    <@@ %%regularArgs |> List.ofArray @@>,
                    <@@
                        %%Utils.createReferenceSetCall (Expr.Var rangeVar) <@@ unbox<'TRange> (%%Expr.Var xVar: obj list).Head @@>

                        %%Utils.createReferenceSetCall (Expr.Var argsVar) <@@ (%%Expr.Var xVar: obj list).Tail |> Array.ofList @@>

                        %% Utils.createDereferenceCall(Expr.Var argsVar)
                        |> Array.iteri (setupArgument (%%(Expr.Var kernelVar): IKernel).Kernel)
                    @@>
                )
            )
            |> fun kernelPrepare ->
                <@ %%kernelPrepare: IKernel -> 'TRange ref -> obj[] ref -> 'TRange -> 'a @>
                    .Compile()

        | _ -> failwithf $"Invalid kernel expression. Must be lambda, but given\n{newLambda}"

    /// Gets internal representation of Program specific to OpenCL.Net.
    member this.Program = program

    /// Gets compiled OpenCL kernel code.
    member this.Code = clCode

    /// Gets kernel lambda after transformation.
    member this.Lambda = newLambda

    /// Gets OpenCL context.
    member this.ClContext = ctx

    member internal this.KernelPrepare = kernelPrepare
