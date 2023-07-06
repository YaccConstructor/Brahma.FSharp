module Brahma.FSharp.Tests.Translator.LocalMemory

open Brahma.FSharp
open Brahma.FSharp.Tests.Translator.Common
open System.IO
open Expecto

let private basePath = Path.Combine("Translator", "BinaryOperations", "Expected")

let private localMemoryTests translator =
    [ let inline createTest name =
          Helpers.createTest translator basePath name

      <@
          fun (range: Range1D) ->
              let mutable x = local ()
              x <- 0
      @>
      |> createTest "Local int" "LocalMemory.int.cl"

      <@
          fun (range: Range1D) ->
              let mutable x = local ()
              x <- 0.0
      @>
      |> createTest "Local float" "LocalMemory.float.cl"

      <@
          fun (range: Range1D) ->
              let xs = localArray 5
              xs.[range.LocalID0] <- range.LocalID0
      @>
      |> createTest "Local int array" "LocalMemory.int [].cl" ]

let tests translator =
    localMemoryTests translator
    |> testList "LocalMemory"
