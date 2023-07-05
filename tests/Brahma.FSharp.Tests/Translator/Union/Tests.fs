module Brahma.FSharp.Tests.Translator.Union.Tests

open Expecto
open Brahma.FSharp.OpenCL.Translator
open Brahma.FSharp.OpenCL.AST
open Brahma.FSharp.OpenCL.Printer
open System.IO
open Brahma.FSharp.Tests.Translator.Common

type TranslateTest =
    | A of int * float
    | B of double
    | C

let unionTests (translator: FSQuotationToOpenCLTranslator) =
    let testGen testCase name (types: List<System.Type>) outFile expectedFile =
        testCase name <| fun () ->
            let context = TranslationContext.Create(TranslatorOptions())
            for type' in types do Type.translateUnion type' |> State.run context |> ignore

            let unions = context.CStructDecls.Values |> Seq.map StructDecl |> Seq.toList

            let ast = AST <| List.map (fun du -> du :> ITopDef<_>) unions
            let code = AST.print ast

            File.WriteAllText(outFile, code) // TODO()

            Utils.filesAreEqual outFile
            <| Path.Combine(basePath, expectedFile)

    [
        testGen testCase "Test 1" [ typeof<TranslateTest> ] "Translation.Test1.gen" "Translation.Test1.cl"
    ]

type SimpleUnion =
    | SimpleOne
    | SimpleTwo of int

type OuterUnion =
    | Outer of int
    | Inner of SimpleUnion

let collectUnionTests (translator: FSQuotationToOpenCLTranslator) =
    let testGen testCase name expected command =
        testCase name <| fun () ->
            let unions =
                Body.translate command
                |> State.exec (TranslationContext.Create(TranslatorOptions()))
                |> fun context -> context.CStructDecls.Keys

            Expect.sequenceEqual unions expected "Should be equal"

    [
        testGen testCase "Simple union" [| typeof<SimpleUnion> |]
            <@ let x = SimpleOne
               let y = SimpleTwo 2
               ()
            @>

        testGen testCase "Nested union 1" [| typeof<SimpleUnion>; typeof<OuterUnion> |]
            <@ let x = Outer 5
               ()
            @>

        testGen testCase "Nested union 2" [| typeof<SimpleUnion>; typeof<OuterUnion> |]
            <@ let x = Inner SimpleOne
               ()
            @>
    ]
