module Brahma.FSharp.Tests.Translator.QuatationTransformation.WorkSize

open Brahma.FSharp
open Brahma.FSharp.OpenCL.Translator.QuotationTransformers
open Expecto

// Duplicate implementation
module Helpers =
    let _localID0 = Unchecked.defaultof<int>

    let _globalSize0 = Unchecked.defaultof<int>
    let _globalSize1 = Unchecked.defaultof<int>
    let _globalSize2 = Unchecked.defaultof<int>

    let _localSize0 = Unchecked.defaultof<int>
    let _localSize1 = Unchecked.defaultof<int>
    let _localSize2 = Unchecked.defaultof<int>

let private workSizeTests =
    [ let createTest name  =
          Common.Helpers.createMapTestAndCompareAsStrings WorkSize.get name

      createTest "Test 1D. Global"
      <| <@
          fun (ndRange: Range1D) ->
              let fst = ndRange.GlobalWorkSize

              ()
      @>
      <| <@
          fun (ndRange: Range1D) ->
              let fst = Helpers._globalSize0

              ()
      @>

      createTest "Test 2D. Global"
      <| <@
          fun (ndRange: Range2D) ->
              let (fst, snd) = ndRange.GlobalWorkSize

              ()
      @>
      <| <@
          fun (ndRange: Range2D) ->
              let fst = Helpers._globalSize0
              let snd = Helpers._globalSize1

              ()
      @>

      createTest "Test 3D. Global"
      <| <@
          fun (ndRange: Range3D) ->
              let (fst, snd, thd) = ndRange.GlobalWorkSize

              ()
      @>
      <| <@
          fun (ndRange: Range3D) ->
              let fst = Helpers._globalSize0
              let snd = Helpers._globalSize1
              let thd = Helpers._globalSize2

              ()
      @>

      createTest "Test 1D. Local"
      <| <@
          fun (ndRange: Range1D) ->
              let fst = ndRange.LocalWorkSize

              ()
      @>
      <| <@
          fun (ndRange: Range1D) ->
              let fst = Helpers._localSize0

              ()
      @>

      createTest "Test 2D. Local"
      <| <@
          fun (ndRange: Range2D) ->
              let (fst, snd) = ndRange.LocalWorkSize

              ()
      @>
      <| <@
          fun (ndRange: Range2D) ->
              let fst = Helpers._localSize0
              let snd = Helpers._localSize1

              ()
      @>

      createTest "Test 3D. Local"
      <| <@
          fun (ndRange: Range3D) ->
              let (fst, snd, thd) = ndRange.LocalWorkSize

              ()
      @>
      <| <@
          fun (ndRange: Range3D) ->
              let fst = Helpers._localSize0
              let snd = Helpers._localSize1
              let thd = Helpers._localSize2

              ()
      @> ]

let tests = workSizeTests |> testList "WorkSize" |> testSequenced
