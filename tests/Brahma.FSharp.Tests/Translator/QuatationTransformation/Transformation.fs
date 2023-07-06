module Brahma.FSharp.Tests.Translator.QuatationTransformation.Transformation

open Expecto
open Brahma.FSharp
open FSharp.Quotations
open Common
open Expecto

let private quotationTransformerTest translator =
    let assertMethodListsEqual (actual: list<Var * Expr>) (expected: list<Var * Expr>) =
        Expect.equal actual.Length expected.Length "List sizes should be equal"
        List.iter2 assertMethodEqual actual expected

    let makeMethods (expr: Expr) =
        let rec go (expr: Expr) =
            match expr with
            | Patterns.Let(var, body, inExpr) ->
                let methods, kernel = go inExpr
                (var, body) :: methods, kernel
            | _ -> [], expr

        let methods, kernelExpr = go expr
        kernelExpr, methods

    let genTest testCase name expr expected =
        let expectedKernelExpr, expectedMethods = makeMethods expected

        testCase name
        <| fun _ ->
            let (actualKernelExpr, actualKernelMethods) =
                expr |> openclTransformQuotation translator

            assertMethodListsEqual actualKernelMethods expectedMethods
            assertExprEqual actualKernelExpr expectedKernelExpr "kernels not equals"

    [ genTest
          testCase
          "Test 0"
          <@
              fun (range: Range1D) (buf: array<int>) ->
                  let mutable x = 1
                  let f y = x <- y
                  f 10
                  buf.[0] <- x
          @>
          <@
              let f (xRef: _ ref) (y: int) = xRef.Value <- y

              fun (range: Range1D) (buf: array<int>) ->
                  let mutable x = 1
                  let xRef = ref x

                  f xRef 10
                  buf.[0] <- xRef.Value
          @>

      genTest
          testCase
          "Test 1"
          <@
              fun (range: Range1D) (buf: array<int>) ->
                  let mutable x = 1
                  let f y = x <- x + y
                  f 10
                  buf.[0] <- x
          @>
          <@
              let f (xRef: _ ref) (y: int) = xRef.Value <- xRef.Value + y

              fun (range: Range1D) (buf: array<int>) ->
                  let mutable x = 1
                  let xRef = ref x

                  f xRef 10
                  buf.[0] <- xRef.Value
          @>

      genTest
          testCase
          "Test 2: simple lambda lifting without capturing variables"
          <@
              fun (range: Range1D) ->
                  let f x =
                      let g y = y + 1
                      g x

                  f 2
          @>
          <@
              let g y = y + 1
              let f x = g x
              fun (range: Range1D) -> f 2
          @>

      genTest
          testCase
          "Test 3: simple lambda lifting with capturing variables"
          <@
              fun (range: Range1D) ->
                  let f x =
                      let g y = y + x
                      g (x + 1)

                  f 2
          @>
          <@
              let g x y = y + x
              let f x = g x (x + 1)
              fun (range: Range1D) -> f 2
          @>

      genTest
          testCase
          "Test 4"
          <@
              fun (range: Range1D) (arr: array<int>) ->
                  let x =
                      let mutable y = 0

                      let addToY x = y <- y + x

                      for i in 0..10 do
                          addToY arr.[i]

                      y

                  x
          @>
          <@
              let addToY (yRef: _ ref) x = yRef.Value <- yRef.Value + x

              let x1UnitFunc (arr: array<int>) =
                  let y = 0
                  let yRef = ref y

                  for i in 0..10 do
                      addToY yRef arr.[i]

                  yRef.Value

              fun (range: Range1D) (arr: array<int>) ->
                  let x1 = x1UnitFunc arr
                  x1
          @>

      genTest
          testCase
          "Test 5"
          <@
              fun (range: Range1D) (arr: array<int>) ->
                  let mutable x = if 0 > 1 then 2 else 3

                  let mutable y =
                      for i in 0..10 do
                          x <- x + 1

                      x + 1

                  let z = x + y

                  let f () = arr.[0] <- x + y + z
                  f ()
          @>
          <@
              let xUnitFunc () = if 0 > 1 then 2 else 3

              let yUnitFunc (xRef: _ ref) =
                  for i in 0..10 do
                      xRef.Value <- xRef.Value + 1

                  xRef.Value + 1

              let f (arr: array<int>) (xRef: _ ref) (yRef: _ ref) z = arr.[0] <- xRef.Value + yRef.Value + z

              fun (range: Range1D) (arr: array<int>) ->
                  let mutable x = xUnitFunc ()
                  let xRef = ref x

                  let mutable y = yUnitFunc xRef
                  let yRef = ref y

                  let z = xRef.Value + yRef.Value

                  f arr xRef yRef z
          @> ]

let tests translator =
    quotationTransformerTest translator
    |> testList "QuotationTransformer"
