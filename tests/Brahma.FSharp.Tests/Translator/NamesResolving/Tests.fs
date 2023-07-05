module Brahma.FSharp.Tests.Translator.NamesResolving.Tests

open Brahma.FSharp
open Expecto
open Brahma.FSharp.Tests.Translator.Common

let namesResolvingTests translator = [
    let inline checkCode cmd outFile expected = checkCode translator cmd outFile expected

    testCase "Bindings with equal names." <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let x = 2
                buf.[0] <- x
                let x = 3
                buf.[1] <- x
            @>

        checkCode command "Bindings.With.Equal.Names.gen" "Bindings.With.Equal.Names.cl"

    testCase "Binding and FOR counter conflict 1." <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let i = 2

                for i in 1 .. 2 do
                    buf.[1] <- i
            @>

        checkCode command "Binding.And.FOR.Counter.Conflict.1.gen" "Binding.And.FOR.Counter.Conflict.1.cl"

    testCase "Binding and FOR counter conflict 2." <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                for i in 1 .. 2 do
                    let i = 2
                    buf.[1] <- i
            @>

        checkCode command "Binding.And.FOR.Counter.Conflict.2.gen" "Binding.And.FOR.Counter.Conflict.2.cl"

    testCase "Binding and FOR counter conflict 3." <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                for i in 0 .. 1 do
                    let i = i + 2
                    buf.[i] <- 2
            @>

        checkCode command "Binding.And.FOR.Counter.Conflict.3.gen" "Binding.And.FOR.Counter.Conflict.3.cl"

    testCase "Binding and FOR counter conflict 4." <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let i = 1

                for i in 0 .. i + 1 do
                    let i = i + 2
                    buf.[i] <- 2
            @>

        checkCode command "Binding.And.FOR.Counter.Conflict.4.gen" "Binding.And.FOR.Counter.Conflict.4.cl"
]
