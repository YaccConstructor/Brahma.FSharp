module Brahma.FSharp.Tests.Translator.Common

open Expecto
open System.IO
open Brahma.FSharp.OpenCL.Printer
open Brahma.FSharp.OpenCL.Translator
open FSharp.Quotations

[<RequireQualifiedAccess>]
module Helpers =
    let openclTranslate (translator: FSQuotationToOpenCLTranslator) (expr: Expr) =
        translator.Translate expr |> fst |> AST.print

    let compareCodeAndFile actualCode pathToExpectedCode =
        let expectedCode =
            (File.ReadAllText pathToExpectedCode).Trim().Replace("\r\n", "\n")

        let actualCode = (actualCode: string).Trim().Replace("\r\n", "\n")

        Expect.equal actualCode expectedCode <| "Code must be the same."

    let checkCode translator quotation pathToExpectedCode =
        let actualCode = quotation |> openclTranslate translator

        compareCodeAndFile actualCode pathToExpectedCode

    let printfStandard code =
        let translator = FSQuotationToOpenCLTranslator.CreateDefault()

        openclTranslate translator code
        |> fun code -> code.Trim().Replace("\r\n", "\n")
        |> printfn "%A"

    // create tests*
    let inline createTest translator basePath name expectedFileName quotation =
        test name { checkCode translator quotation <| Path.Combine(basePath, expectedFileName) }

    let inline createPTest name = ptest name { () }
