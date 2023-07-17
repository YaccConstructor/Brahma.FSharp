module Brahma.FSharp.Tests.Translator.QuatationTransformation.Variables

open Brahma.FSharp.OpenCL.Translator.QuotationTransformers
open Expecto

let private uniquesTests =
    [ let createTest name source expected =
          test name {
              let actual = Variables.defsToLambda source

              let actualStr = actual.ToString()
              let expectedStr = expected.ToString()

              Expect.equal actualStr expectedStr "Result should be the same."
          }

      createTest "Test 1." <| <@ let x = 1 + 1 in () @> <| <@ let x = 1 + 1 in () @>

      createTest "Test 2."
      <| <@
          let x =
              let mutable y = 0

              for i in 1..10 do
                  y <- y + i

              y

          x
      @>
      <| <@
          let x =
              let xUnitFunc =
                  fun (unitVar: unit) ->
                      let mutable y = 0

                      for i in 1..10 do
                          y <- y + i

                      y

              xUnitFunc ()

          x
      @>

      createTest "Test 3."
      <| <@
          let x =
              let mutable y =
                  if true then
                      let z = 10
                      z + 1
                  else
                      let z = 20
                      z + 2

              for i in 1..10 do
                  let z = if false then 10 else 20
                  y <- y + i + z

              y

          x
      @>
      <| <@
          let x =
              let xUnitFunc =
                  fun (unitVar: unit) ->
                      let mutable y =
                          let yUnitFunc =
                              fun (unitVar: unit) ->
                                  if true then
                                      let z = 10
                                      z + 1
                                  else
                                      let z = 20
                                      z + 2

                          yUnitFunc ()

                      for i in 1..10 do
                          let z =
                              let zUnitFunc = fun (unitVar: unit) -> if false then 10 else 20
                              zUnitFunc ()

                          y <- y + i + z

                      y

              xUnitFunc ()

          x
      @> ]

let tests = uniquesTests |> testList "Variables" |> testSequenced
