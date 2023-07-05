module Brahma.FSharp.Tests.Translator.Common

open Expecto
open Brahma.FSharp.Tests
open System.IO
open Brahma.FSharp.OpenCL.Printer
open Brahma.FSharp.OpenCL.Translator
open FSharp.Quotations

[<RequireQualifiedAccess>]
module Helpers =
    let basePath = "TranslationTests/Expected/"

    let openclTranslate (translator: FSQuotationToOpenCLTranslator) (expr: Expr) =
        translator.Translate expr
        |> fst
        |> AST.print

    let checkCode translator command outFile expected =
        let code = command |> openclTranslate translator

        let expectedPath = Path.Combine(basePath, expected)
        // read from file

        Utils.filesAreEqual "targetPath" expectedPath

