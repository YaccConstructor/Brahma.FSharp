module Brahma.FSharp.Tests.Translator.LocalId.Tests

open Brahma.FSharp
open Expecto
open Brahma.FSharp.Tests.Translator.Common

let basicLocalIdTests translator = [
    let inline checkCode cmd outFile expected = Helpers.checkCode translator cmd outFile expected

    testCase "LocalID of 1D" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let id = range.LocalID0
                buf.[id] <- 0
            @>

        checkCode command "LocalID1D.gen" "LocalID1D.cl"

    testCase "LocalID of 2D" <| fun _ ->
        let command =
            <@ fun (range: Range2D) (buf: int clarray) ->
                let v = range.LocalID0
                let id = range.LocalID1
                buf.[id] <- v
            @>

        checkCode command "LocalID2D.gen" "LocalID2D.cl"
]
