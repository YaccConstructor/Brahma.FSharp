module Brahma.FSharp.Tests.Translator.Carrying.Tests

open Brahma.FSharp
open Expecto
open Brahma.FSharp.Tests.Translator.Common

let curryingTests translator = [
    let inline checkCode cmd outFile expected = checkCode translator cmd outFile expected

    testCase "Nested functions.Carring 1." <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f x y = x - y
                let g = f 2
                buf.[0] <- g 3
                buf.[1] <- g 5
            @>

        checkCode command "Nested.Function.Carring.gen" "Nested.Function.Carring.cl"

    testCase "Nested functions.Currying 2." <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f x y =
                    let gg = ref 0

                    for i in 1 .. x do
                        gg := !gg + y

                    !gg

                let g x = f 2 x
                buf.[0] <- g 2
                buf.[1] <- g 3
            @>

        checkCode command "Nested.Function.Carring2.gen" "Nested.Function.Carring2.cl"
]
