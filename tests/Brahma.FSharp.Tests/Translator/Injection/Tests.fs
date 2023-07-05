module Brahma.FSharp.Tests.Translator.Injection.Tests

open Expecto
open Brahma.FSharp
open Brahma.FSharp.Tests.Translator.Common

let quotationsInjectionTests translator = [
    let inline checkCode cmd outFile expected = Helpers.checkCode translator cmd outFile expected

    testCase "Quotations injections 1" <| fun _ ->
        let myF = <@ fun x -> x * x @>

        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                buf.[0] <- (%myF) 2
                buf.[1] <- (%myF) 4
            @>

        checkCode command "Quotations.Injections.1.gen" "Quotations.Injections.1.cl"

    testCase "Quotations injections 2" <| fun _ ->
        let myF = <@ fun x y -> x - y @>

        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                buf.[0] <- (%myF) 2 3
                buf.[1] <- (%myF) 4 5
            @>

        checkCode command "Quotations.Injections.2.gen" "Quotations.Injections.2.cl"

]
