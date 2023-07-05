module Brahma.FSharp.Tests.Translator.Barrier.Tests

open Expecto
open Brahma.FSharp
open Brahma.FSharp.Tests.Translator.Common

let barrierTests translator = [
    let inline checkCode cmd outFile expected = Helpers.checkCode translator cmd outFile expected

    testCase "Local barrier translation tests" <| fun () ->
        let command = <@ fun (range: Range1D) -> barrierLocal () @>
        checkCode command "Barrier.Local.gen" "Barrier.Local.cl"

    testCase "Global barrier translation tests" <| fun () ->
        let command = <@ fun (range: Range1D) -> barrierGlobal () @>
        checkCode command "Barrier.Global.gen" "Barrier.Global.cl"

    testCase "Full barrier translation tests" <| fun () ->
        let command = <@ fun (range: Range1D) -> barrierFull () @>
        checkCode command "Barrier.Full.gen" "Barrier.Full.cl"
]
