module Brahma.FSharp.Tests.Translator.BinOp

open Brahma.FSharp
open Brahma.FSharp.Tests.Translator.Common
open System.IO
open Expecto

let private basePath = Path.Combine("Translator", "BinOp", "Expected")

let private basicBinOpsTests translator =
    [ let inline createTest name =
          Helpers.createTest translator basePath name

      <@ fun (range: Range1D) (buf: int clarray) -> buf.[0] <- 0 @>
      |> createTest "Array item set" "Array.Item.Set.cl"

      <@
          fun (range: Range1D) (buf: int clarray) ->
              let x = 1
              buf.[0] <- x
      @>
      |> createTest "Binding" "Binding.cl"

      <@ fun (range: Range1D) (buf: int clarray) -> buf.[0] <- 1 + 2 @>
      |> createTest "Binop plus" "Binop.Plus.cl"

      <@
          fun (range: Range1D) (buf: int clarray) ->
              let x = 0
              let y = x + 1
              let z = y * 2
              let a = z - x
              let i = a / 2
              buf.[0] <- i
      @>
      |> createTest "Binary operations. Math." "Binary.Operations.Math.cl"

      <@
          fun (range: Range1D) (buf: float clarray) ->
              let tempVarY = 1.
              buf.[0] <- max buf.[0] tempVarY
              buf.[0] <- max buf.[0] tempVarY
      @>
      |> createTest "TempVar from MAX transformation should not affect other variables" "MAX.Transformation.cl" ]

let tests translator =
    basicBinOpsTests translator |> testList "BinaryOperations"
