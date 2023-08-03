namespace Brahma.FSharp

open Brahma.FSharp
open Microsoft.FSharp.Control
open OpenCL.Net
open FSharp.Quotations.Evaluator
open Brahma.FSharp.OpenCL.Shared
open Brahma.FSharp.OpenCL.Translator.QuotationTransformers

/// Represents an abstraction over OpenCL kernel.
type ClKernel<'TRange, 'a when 'TRange :> INDRange>(program: ClProgram<'TRange, 'a>, ?kernelName) =

    let kernelName = defaultArg kernelName "brahmaKernel"

    let kernel =
        let (clKernel, error) = Cl.CreateKernel(program.Program, kernelName)

        if error <> ErrorCode.Success then
            failwithf $"OpenCL kernel creation problem. Error: %A{error}"

        clKernel

    let args = ref [||]
    let range = ref Unchecked.defaultof<'TRange>

    /// Gets lambda function needed to pass parameters to kernel.
    member this.KernelFunc = program.KernelPrepare (this :> IKernel) range args

    interface IKernel with
        member this.Kernel = kernel
        member this.NDRange = range.Value :> INDRange

[<AutoOpen>]
module ClProgramExtensions =
    type ClProgram<'TRange, 'a when 'TRange :> INDRange> with

        /// Returns new kernel instance corresponding to the given OpenCL program.
        member this.GetKernel() = ClKernel(this)
