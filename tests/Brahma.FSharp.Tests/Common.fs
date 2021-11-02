namespace Brahma.FSharp.Tests

open System.IO
open Brahma.FSharp.OpenCL
open Expecto
open Brahma.FSharp.OpenCL.Translator
open Brahma.FSharp.OpenCL.Printer.AST
open FSharp.Quotations

[<AutoOpen>]
module Common =
    let context =
        let deviceType = ClDeviceType.Default
        let platformName = ClPlatform.Any
        ClContext(platformName, deviceType)

module CustomDatatypes =
    [<Struct>]
    type WrappedInt =
        val mutable InnerValue: int
        new(x) = { InnerValue = x }

        static member (+) (x: WrappedInt, y: WrappedInt) =
            WrappedInt(x.InnerValue + y.InnerValue)

        static member (-) (x: WrappedInt, y: WrappedInt) =
            WrappedInt(x.InnerValue - y.InnerValue)

module Utils =
    let filesAreEqual file1 file2 =
        let all1 =
            (File.ReadAllText file1)
                .Trim()
                .Replace("\r\n", "\n")

        let all2 =
            (File.ReadAllText file2)
                .Trim()
                .Replace("\r\n", "\n")

        Expect.equal all1 all2 "Files should be equals as strings"

    let openclCompile (command: Expr<('a -> 'b)>) =
        let kernel = context.CreateClKernel command
        kernel.Code

    let openclTranslate (expr: Expr) =
        let translator = FSQuotationToOpenCLTranslator()
        let (ast, methods) = translator.Translate(expr)
        print ast

    let openclTransformQuotation (expr: Expr) =
        QuotationTransformers.Transformer.transformQuotation expr []
