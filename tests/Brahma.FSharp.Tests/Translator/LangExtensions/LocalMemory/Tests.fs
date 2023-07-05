module Brahma.FSharp.Tests.Translator.LocalMemory.Tests

open Expecto
open Brahma.FSharp
open Brahma.FSharp.Tests.Translator.Common

let localMemoryTests translator = [
    let inline checkCode cmd outFile expected = Helpers.checkCode translator cmd outFile expected

    testCase "Local int" <| fun _ ->
        let command =
            <@ fun (range: Range1D) ->
                let mutable x = local ()
                x <- 0
            @>

        checkCode command "LocalMemory.int.gen" "LocalMemory.int.cl"

    testCase "Local float" <| fun _ ->
        let command =
            <@ fun (range: Range1D) ->
                let mutable x = local ()
                x <- 0.0
            @>

        checkCode command "LocalMemory.float.gen" "LocalMemory.float.cl"

    testCase "Local int array" <| fun _ ->
        let command =
            <@ fun (range: Range1D) ->
                let xs = localArray 5
                xs.[range.LocalID0] <- range.LocalID0
            @>

        checkCode command "LocalMemory.int [].gen" "LocalMemory.int [].cl"
]
