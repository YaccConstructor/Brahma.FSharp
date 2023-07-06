module Brahma.FSharp.Tests.Translator.QuatationTransformation.LambdaLifting

open Expecto
open Brahma.FSharp.OpenCL.Translator.QuotationTransformers
open Common
open Expecto

let private lambdaLiftingTests =
    let genParameterLiftTest name expr expected =
        test name {
            let actual = LambdaLifting.parameterLiftExpr expr

            assertExprEqual actual expected equalsMessage
        }

    [ genParameterLiftTest
          "Test 1"
          <@
              let x = 1
              let addToX y = x + y
              addToX 2
          @>
          <@
              let x = 1
              let addToX x y = x + y
              addToX x 2
          @>

      genParameterLiftTest
          "Test 2"
          <@
              let x = 1
              let z = x

              let addToX y = // freeVars: [x, z]
                  x + y + z

              let f z1 = // freeVars: [], addToX freeVars: [x, z]
                  2 + addToX z1

              f 3
          @>
          <@
              let x = 1
              let z = x

              let addToX x z y = x + y + z
              let f x z z1 = 2 + addToX x z z1
              f x z 3
          @>

      genParameterLiftTest
          "Test 3"
          <@
              let mainX = "global variable"
              let mainY = "global variable"
              let mainZ = "global variable"

              let foo fooX =
                  let fooY = "local variable of foo"
                  let bar barX = mainX + fooY + barX
                  bar fooX + mainY

              foo mainZ
          @>
          <@
              let mainX = "global variable"
              let mainY = "global variable"
              let mainZ = "global variable"

              let foo mainX mainY fooX =
                  let fooY = "local variable of foo"
                  let bar fooY mainX barX = mainX + fooY + barX
                  bar fooY mainX fooX + mainY

              foo mainX mainY mainZ
          @>

      genParameterLiftTest
          "Test 4"
          <@
              let x0 = 0

              let f x1 =
                  let g x2 =
                      let h x3 = x3 + x0
                      h x2

                  g x1

              f x0
          @>
          <@
              let x0 = 0

              let f x0 x1 =
                  let g x0 x2 =
                      let h x0 x3 = x3 + x0
                      h x0 x2

                  g x0 x1

              f x0 x0
          @> ]

let tests _ =
    lambdaLiftingTests
    |> testList "Lambda lifting"

