module Brahma.FSharp.Tests.Translator.Barrier

open Brahma.FSharp
open Brahma.FSharp.Tests.Translator.Common
open System.IO
open Expecto

let private basePath = Path.Combine("Translator", "BinaryOperations", "Expected")

let private barrierTests translator =
    [ let inline createTest name =
          Helpers.createTest translator basePath name

      <@ fun (range: Range1D) -> barrierLocal () @>
      |> createTest "Local barrier translation tests" "Barrier.Local.cl"

      <@ fun (range: Range1D) -> barrierGlobal () @>
      |> createTest "Global barrier translation tests" "Barrier.Global.cl"

      <@ fun (range: Range1D) -> barrierFull () @>
      |> createTest "Full barrier translation tests" "Barrier.Full.cl" ]

let tests translator =
    barrierTests translator |> testList "Barrier"
