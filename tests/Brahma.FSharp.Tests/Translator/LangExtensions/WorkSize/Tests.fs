module Brahma.FSharp.Tests.Translator.WorkSize.Tests

open Brahma.FSharp
open Expecto
open Brahma.FSharp.Tests.Translator.Common

let basicWorkSizeTests translator = [
    let inline checkCode cmd outFile expected = Helpers.checkCode translator cmd outFile expected

    testCase "WorkSize of 1D" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf: int clarray) ->
                    let gSize = range.GlobalWorkSize
                    let lSize = range.LocalWorkSize
                    ()
            @>

        checkCode command "WorkSize1D.gen" "WorkSize1D.cl"

    testCase "WorkSize of 2D" <| fun _ ->
        let command =
            <@
                fun (range: Range2D) (buf: int clarray) ->
                    let (gSizeX, gSizeY) = range.GlobalWorkSize
                    let (lSizeX, lSizeY) = range.LocalWorkSize
                    ()
            @>

        checkCode command "WorkSize2D.gen" "WorkSize2D.cl"

    testCase "WorkSize of 3D" <| fun _ ->
        let command =
           <@
                fun (range: Range3D) (buf: int clarray) ->
                    let (gSizeX, gSizeY, gSizeZ) = range.GlobalWorkSize
                    let (lSizeX, lSizeY, lSizeZ) = range.LocalWorkSize
                    ()
            @>

        checkCode command "WorkSize3D.gen" "WorkSize3D.cl"
]
