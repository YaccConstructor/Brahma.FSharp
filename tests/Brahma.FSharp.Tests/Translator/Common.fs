module Brahma.FSharp.Tests.Translator.Common

open Expecto
open System.IO
open Brahma.FSharp.OpenCL.Printer
open Brahma.FSharp.OpenCL.Translator
open FSharp.Quotations

[<RequireQualifiedAccess>]
module Helpers =
    let openclTranslate (expr: Expr) =
        FSQuotationToOpenCLTranslator.CreateDefault().Translate expr |> fst |> AST.print

    let filterText (text: string) = text.Trim().Replace("\r\n", "\n")

    let compareCodeAndFile actualCode pathToExpectedCode =
        let expectedCode = (File.ReadAllText pathToExpectedCode) |> filterText

        let actualCode = (actualCode: string).Trim().Replace("\r\n", "\n") |> filterText

        Expect.equal actualCode expectedCode <| "Code must be the same."

    let checkCode quotation pathToExpectedCode =
        let actualCode = quotation |> openclTranslate

        compareCodeAndFile actualCode pathToExpectedCode

    // create tests*
    let inline createTest basePath name expectedFileName quotation =
        test name { checkCode quotation <| Path.Combine(basePath, expectedFileName) }

    let inline createPTest name = ptest name { () }
