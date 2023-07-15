module Brahma.FSharp.Tests.Translator.LangExtensions.WorkSize

open Brahma.FSharp
open Brahma.FSharp.Tests.Translator.Common
open System.IO
open Expecto

let private basePath =
    Path.Combine("Translator", "LangExtensions", "WorkSize", "Expected")

let private basicWorkSizeTests =
    [ let inline createTest name = Helpers.createTest basePath name

      <@
          fun (range: Range1D) (buf: int clarray) ->
              let gSize = range.GlobalWorkSize
              let lSize = range.LocalWorkSize
              ()
      @>
      |> createTest "WorkSize of 1D" "WorkSize1D.cl"

      <@
          fun (range: Range2D) (buf: int clarray) ->
              let (gSizeX, gSizeY) = range.GlobalWorkSize
              let (lSizeX, lSizeY) = range.LocalWorkSize
              ()
      @>
      |> createTest "WorkSize of 2D" "WorkSize2D.cl"

      <@
          fun (range: Range3D) (buf: int clarray) ->
              let (gSizeX, gSizeY, gSizeZ) = range.GlobalWorkSize
              let (lSizeX, lSizeY, lSizeZ) = range.LocalWorkSize
              ()
      @>
      |> createTest "WorkSize of 3D" "WorkSize3D.cl" ]

let tests = basicWorkSizeTests |> testList "BasicWorkSize"
