module Brahma.FSharp.Tests.Translator.ConstantArray

open Brahma.FSharp
open Brahma.FSharp.Tests.Translator.Common
open System.IO
open Expecto

let private basePath = Path.Combine("Translator", "ConstantArray", "Expected")

let private constantArrayTests =
    [ let inline createTest name = Helpers.createTest basePath name

      let cArray1 =
          [| 1
             2
             3 |]

      <@ fun (range: Range1D) (buf: int clarray) -> buf.[0] <- cArray1.[1] @>
      |> createTest "Constant array translation. Test 1" "Constant array translation. Test 1.cl"

      let cArray1 =
          [| 1
             2
             3 |]

      <@ fun (range: Range1D) (buf: int clarray) -> buf.[0] <- 1 + cArray1.[1] @>
      |> createTest "Constant array translation. Test 2" "Constant array translation. Test 2.cl" ]

let tests = constantArrayTests |> testList "ConstantArray"
