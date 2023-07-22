module Brahma.FSharp.Tests.Translator.QuatationTransformation.LambdaLifting

open Expecto
open Brahma.FSharp.OpenCL.Translator.QuotationTransformers
open Common
open Microsoft.FSharp.Quotations
open Brahma.FSharp

let private parameterLiftingTests =
    let createTest name expr expected =
        test name {
            let actual = Lift.Parameters.lift expr

            let actual = actual.ToString()
            let expected = expected.ToString()

            Expect.equal actual expected equalsMessage
        }

    [ createTest
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

      createTest
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

      createTest
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

      createTest
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
    |> testList "Parameter lifting"

let unitVar name = Expr.Cast<unit>(Expr.Var(Var(name, typeof<unit>)))

let unitCleanUpTests =
    let createTest name expr expected =
        test name {
            let actual = Lift.UnitArguments.cleanUp expr

            equalAsStrings actual expected equalsMessage
        }

    [ createTest "Test 1"
      <| <@ let f (x: unit) = x in () @>
      <| <@ let f (x: unit) = x in () @>

      createTest "Test 2"
      <| <@ let f (x: unit) (y: int) = x in () @>
      <| <@ let f (y: int) = (%unitVar "x") in () @>

      createTest "Test 3"
      <| <@ let f (x: unit) (y: unit) = x in () @>
      <| <@ let f = (%unitVar "x") in () @>

      createTest "Test 4"
      <| <@ let f (x: int) = x in () @>
      <| <@ let f (x: int) = x in () @>

      createTest "Test 5"
      <| <@ let f (x: int option) = x in () @>
      <| <@ let f (x: int option) = x in () @>

      createTest "Test 6"
      <| <@ let f (x: unit option) = x in () @>
      <| <@ let f (x: unit option) = x in () @>

      createTest "Test 7"
      <| <@ let f (x: unit) (y: unit) (z: unit) = if x = y then z else y in () @>
      <| <@ let f = if (%unitVar "x") = (%unitVar "y") then (%unitVar "z") else (%unitVar "y") in () @>

      createTest "Test 8"
      <| <@ let f (x: unit) = let g (y: unit) = Some () in () in () @>
      <| <@ let f (x: unit) = let g (y: unit) = Some () in () in () @>

      createTest "Test 9"
      <| <@ let f (x: unit) (y: unit) =
                let g (z: unit) (c: unit) = x in g y x
            in () @>
      <| <@ let f = let g = (%unitVar "x") in g in () @>

      createTest "Test 10"
      <| <@ let f () = printfn "side effect"; ()
            let g (x: unit) (y: unit) (z: int) = z

            // side effect in f application
            g (f ()) () 0 @>
      <| <@ let f () = printfn "side effect"; ()
            let g (z: int) = z

            // no side effect
            g 0 @>

      createTest "Test 11"
      <| <@ let f (x: int) = printfn "side effect"; () in
            let g (x: unit) (y: int) = y in

            // side effect in f application
            g (f 0) 0 @>
      <| <@ let f (x: int) = printfn "side effect"; () in
            let g (y: int) = y in

            // no side effect
            g 0 @>

      createTest "Test 12" // id
      <| <@ let f (x: int) = x in f 4 @>
      <| <@ let f (x: int) = x in f 4 @> ]
    |> testList "Unit clean up"


let tests = [ parameterLiftingTests ]
            |> testList "Lambda lifting"
