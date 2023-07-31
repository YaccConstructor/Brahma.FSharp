module Brahma.FSharp.Tests.Translator.LangExtensions.LocalId

open Brahma.FSharp
open Brahma.FSharp.Tests.Translator.Common
open System.IO
open Expecto

let private basePath =
    Path.Combine("Translator", "LangExtensions", "LocalID", "Expected")

let private basicLocalIdTests =
    [
        let inline createTest name = Helpers.createTest basePath name

        <@
            fun (range: Range1D) (buf: int clarray) ->
                let id = range.LocalID0
                buf.[id] <- 0
        @>
        |> createTest "LocalID of 1D" "LocalID1D.cl"

        <@
            fun (range: Range2D) (buf: int clarray) ->
                let v = range.LocalID0
                let id = range.LocalID1
                buf.[id] <- v
        @>
        |> createTest "LocalID of 2D" "LocalID2D.cl"
    ]

let tests = basicLocalIdTests |> testList "BasicLocalId"
