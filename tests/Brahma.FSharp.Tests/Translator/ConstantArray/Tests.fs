module Brahma.FSharp.Tests.Translator.ConstantArray.Tests

open Expecto
open Brahma.FSharp
open Brahma.FSharp.Tests.Translator.Common

let constantArrayTests translator = [
    let inline checkCode cmd outFile expected = checkCode translator cmd outFile expected

    testCase "Constant array translation. Test 1" <| fun _ ->
        let cArray1 = [| 1; 2; 3 |]
        let command = <@ fun (range: Range1D) (buf: int clarray) -> buf.[0] <- cArray1.[1] @>
        checkCode command "Constant array translation. Test 1.gen" "Constant array translation. Test 1.cl"

    testCase "Constant array translation. Test 2" <| fun _ ->
        let cArray1 = [| 1; 2; 3 |]
        let command = <@ fun (range: Range1D) (buf: int clarray) -> buf.[0] <- 1 + cArray1.[1] @>
        checkCode command "Constant array translation. Test 2.gen" "Constant array translation. Test 2.cl"
]
