module Brahma.FSharp.Tests.Translator.QuatationTransformation.VarDefsToLambda

open Expecto
open Brahma.FSharp.OpenCL.Translator.QuotationTransformers
open Common
open Expecto

let private varDefsToLambdaTest =
    let genVarDefToLambdaTest name expr expected =
        test name {
            let actual = VarDefsToLambdaTransformer.transformVarDefsToLambda expr

            assertExprEqual actual expected equalsMessage
        }

    [ genVarDefToLambdaTest
          "Test 1"
          <@
              let x =
                  let mutable y = 0

                  for i in 1..10 do
                      y <- y + i

                  y

              x
          @>
          <@
              let x =
                  let xUnitFunc () =
                      let mutable y = 0

                      for i in 1..10 do
                          y <- y + i

                      y

                  xUnitFunc ()

              x
          @>

      genVarDefToLambdaTest
          "Test 2: we need to go deeper"
          <@
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
          <@
              let x =
                  let xUnitFunc () =
                      let mutable y =
                          let yUnitFunc () =
                              if true then
                                  let z = 10
                                  z + 1
                              else
                                  let z = 20
                                  z + 2

                          yUnitFunc ()

                      for i in 1..10 do
                          let z =
                              let zUnitFunc () = if false then 10 else 20
                              zUnitFunc ()

                          y <- y + i + z

                      y

                  xUnitFunc ()

              x
          @> ]

let tests _ =
    varDefsToLambdaTest |> testList "Var -> Lambda"
