module Brahma.FSharp.Tests.Translator.LangExtensions.LocalMemory

open Brahma.FSharp
open Brahma.FSharp.Tests.Translator.Common
open System.IO
open Expecto

let private basePath =
    Path.Combine("Translator", "LangExtensions", "LocalMemory", "Expected")

let private localMemoryTests =
    [
        let inline createTest name = Helpers.createTest basePath name

        <@
            fun (range: Range1D) ->
                let mutable x = local ()
                x <- 0
        @>
        |> createTest "Local int" "LocalMemory.int.cl"

        <@
            fun (range: Range1D) ->
                let mutable x = local ()
                x <- 0.0
        @>
        |> createTest "Local float" "LocalMemory.float.cl"

        <@
            fun (range: Range1D) ->
                let xs = localArray 5
                xs.[range.LocalID0] <- range.LocalID0
        @>
        |> createTest "Local int array" "LocalMemory.int [].cl"
    ]

let tests = localMemoryTests |> testList "LocalMemory"
