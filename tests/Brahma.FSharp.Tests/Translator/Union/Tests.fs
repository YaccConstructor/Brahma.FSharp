module Brahma.FSharp.Tests.Translator.Union

open Expecto
open Brahma.FSharp.OpenCL.Translator
open Brahma.FSharp.OpenCL.AST
open Brahma.FSharp.OpenCL.Printer
open System.IO
open Brahma.FSharp.Tests.Translator.Common

let private basePath = Path.Combine("Translator", "Union", "Expected")

type TranslateTest =
    | A of int * float
    | B of double
    | C

let private unionTests =
    let testGen name (types: List<System.Type>) expectedFile =
        test name {
            let context = TranslationContext.Create(TranslatorOptions())

            types
            |> List.iter (fun type' -> Type.translateUnion type' |> State.run context |> ignore)

            context.CStructDecls.Values
            |> Seq.map StructDecl
            |> Seq.toList
            |> List.map (fun du -> du :> ITopDef<_>)
            |> AST
            |> AST.print
            |> fun code -> Helpers.compareCodeAndFile code <| Path.Combine(basePath, expectedFile)
        }

    [ testGen "Test 1" [ typeof<TranslateTest> ] "Translation.Test1.cl" ]

type SimpleUnion =
    | SimpleOne
    | SimpleTwo of int

type OuterUnion =
    | Outer of int
    | Inner of SimpleUnion

let private collectUnionTests =
    let testGen name expected command =
        test name {
            Body.translate command
            |> State.exec (TranslationContext.Create(TranslatorOptions()))
            |> fun context -> context.CStructDecls.Keys
            |> fun unions -> Expect.sequenceEqual unions expected "Should be equal"
        }

    [
        testGen
            "Simple union"
            [| typeof<SimpleUnion> |]
            <@
                let x = SimpleOne
                let y = SimpleTwo 2
                ()
            @>

        testGen
            "Nested union 1"
            [| typeof<SimpleUnion>; typeof<OuterUnion> |]
            <@
                let x = Outer 5
                ()
            @>

        testGen
            "Nested union 2"
            [| typeof<SimpleUnion>; typeof<OuterUnion> |]
            <@
                let x = Inner SimpleOne
                ()
            @>
    ]

let tests = [ unionTests; collectUnionTests ] |> List.concat |> testList "Union"
