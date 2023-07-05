module Brahma.FSharp.Tests.Translator.BinaryOperations.Tests

open Expecto
open Brahma.FSharp
open Brahma.FSharp.Tests.Translator.Common

let basicBinOpsTests translator = [
    let checkCode command = Helpers.checkCode translator command

    testCase "Array item set" <| fun _ ->
        let command = <@ fun (range: Range1D) (buf: int clarray) -> buf.[0] <- 0 @>

        checkCode command "Array.Item.Set.gen" "Array.Item.Set.cl"

    testCase "Binding" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let x = 1
                buf.[0] <- x
            @>

        checkCode command "Binding.gen" "Binding.cl"

    testCase "Binop plus" <| fun _ ->
        let command = <@ fun (range: Range1D) (buf: int clarray) -> buf.[0] <- 1 + 2 @>

        checkCode command "Binop.Plus.gen" "Binop.Plus.cl"

    testCase "Binary operations. Math." <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let x = 0
                let y = x + 1
                let z = y * 2
                let a = z - x
                let i = a / 2
                buf.[0] <- i
            @>

        checkCode command "Binary.Operations.Math.gen" "Binary.Operations.Math.cl"

    testCase "TempVar from MAX transformation should not affect other variables" <| fun () ->
        let command =
            <@
                fun (range: Range1D) (buf: float clarray) ->
                    let tempVarY = 1.
                    buf.[0] <- max buf.[0] tempVarY
                    buf.[0] <- max buf.[0] tempVarY
            @>

        checkCode command "MAX.Transformation.gen" "MAX.Transformation.cl"
]
