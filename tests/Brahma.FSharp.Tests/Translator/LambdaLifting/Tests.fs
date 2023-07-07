module Brahma.FSharp.Tests.Translator.LambdaLifting

open Brahma.FSharp
open Brahma.FSharp.Tests.Translator.Common
open System.IO
open Expecto

let private basePath = Path.Combine("Translator", "LambdaLifting", "Expected")

let private lambdaLiftingTests =
    [ let inline createTest name = Helpers.createTest basePath name

      <@
          fun (range: Range1D) (buf: int clarray) ->
              let f = 3
              buf.[0] <- f
      @>
      |> createTest "Template Let Transformation Test 0" "Template Test 0.cl"

      <@
          fun (range: Range1D) (buf: int clarray) ->
              let f =
                  let x = 3
                  x

              buf.[0] <- f
      @>
      |> createTest "Template Let Transformation Test 1" "Template Test 1.cl"

      <@
          fun (range: Range1D) (buf: int clarray) ->
              let f =
                  let x =
                      let y = 3
                      y

                  x

              buf.[0] <- f
      @>
      |> createTest "Template Let Transformation Test 2" "Template Test 2.cl"

      <@
          fun (range: Range1D) (buf: int clarray) ->
              let f =
                  let f = 5
                  f

              buf.[0] <- f
      @>
      |> createTest "Template Let Transformation Test 3" "Template Test 3.cl"

      <@
          fun (range: Range1D) (buf: int clarray) ->
              let f =
                  let f =
                      let f = 5
                      f

                  f

              buf.[0] <- f
      @>
      |> createTest "Template Let Transformation Test 4" "Template Test 4.cl"

      <@
          fun (range: Range1D) (buf: int clarray) ->
              let f a b =
                  let x y z = y + z
                  x a b

              buf.[0] <- f 1 7
      @>
      |> createTest "Template Let Transformation Test 5" "Template Test 5.cl"

      <@
          fun (range: Range1D) (buf: int clarray) ->
              let f x y =
                  let x = x
                  x + y

              buf.[0] <- f 7 8
      @>
      |> createTest "Template Let Transformation Test 6" "Template Test 6.cl"

      <@
          fun (range: Range1D) (buf: int clarray) ->
              let f y =
                  let x y = 6 - y
                  x y

              buf.[0] <- f 7
      @>
      |> createTest "Template Let Transformation Test 7" "Template Test 7.cl"

      <@
          fun (range: Range1D) (m: int clarray) ->
              let p = m.[0]

              let x n =
                  let l = m.[9]
                  let g k = k + m.[0] + m.[1]

                  let r =
                      let y a =
                          let x = 5 - n + (g 4)
                          let z t = m.[2] + a - t
                          z (a + x + l)

                      y 6

                  r + m.[3]

              m.[0] <- x 7
      @>
      |> createTest "Template Let Transformation Test 8" "Template Test 8.cl"

      <@
          fun (range: Range1D) (buf: int clarray) ->
              let x n =
                  let r = 8
                  let h = r + n
                  h

              buf.[0] <- x 9
      @>
      |> createTest "Template Let Transformation Test 9" "Template Test 9.cl"

      <@
          fun (range: Range1D) (buf: int clarray) ->
              let p = 9

              let x n b =
                  let t = 0
                  n + b + t

              buf.[0] <- x 7 9
      @>
      |> createTest "Template Let Transformation Test 10" "Template Test 10.cl"

      <@
          fun (range: Range1D) (buf: int clarray) ->
              let p = 1

              let m =
                  let r l = l + p
                  r 9

              let z k = k + 1
              buf.[0] <- m
      @>
      |> createTest "Template Let Transformation Test 11" "Template Test 11.cl"

      <@
          fun (range: Range1D) (buf: int clarray) ->
              let f x y =
                  let y = y
                  let y = y
                  let g x m = m + x
                  g x y

              buf.[0] <- f 1 7
      @>
      |> createTest "Template Let Transformation Test 12" "Template Test 12.cl"

      <@
          fun (range: Range1D) (buf: int clarray) ->
              let f y =
                  let y = y
                  let y = y
                  let g m = m + 1
                  g y

              buf.[0] <- f 7
      @>
      |> createTest "Template Let Transformation Test 13" "Template Test 13.cl"

      <@
          fun (range: Range1D) (buf: int clarray) ->
              let f (y: int) =
                  let y = y
                  let y = y

                  let g (m: int) =
                      let g r t = r + y - t
                      let n o = o - (g y 2)
                      n 5

                  g y

              let z y = y - 2
              buf.[0] <- f (z 7)
      @>
      |> createTest "Template Let Transformation Test 14" "Template Test 14.cl"

      <@
          fun (range: Range1D) (buf: int clarray) ->
              let f y =
                  let Argi index = if index = 0 then buf.[1] else buf.[2]
                  Argi y

              buf.[0] <- f 0
      @>
      |> createTest "Template Let Transformation Test 15" "Template Test 15.cl"

      <@
          fun (range: Range1D) (buf: int clarray) ->
              let f y =
                  if y = 0 then
                      let z a = a + 1
                      z 9
                  else
                      buf.[2]

              buf.[0] <- f 0
      @>
      |> createTest "Template Let Transformation Test 16" "Template Test 16.cl"

      <@
          fun (range: Range1D) (buf: int clarray) ->
              let f x =
                  let g = 1 + x
                  g

              buf.[0] <- f 1
      @>
      |> createTest "Let renamed" "Let renamed.cl"

      <@
          fun (range: Range1D) (buf: int clarray) ->
              let f m k =
                  let g q w = 1 + q + w
                  let t p = 7 - p
                  (g 1 2) - m * k / (t 53)

              buf.[0] <- f 1 4
      @>
      |> createTest "Let renamed 2" "Let renamed 2.cl"

      <@
          fun (range: Range1D) (buf: int clarray) ->
              let f x y =
                  let y = y
                  let y = y
                  let g x m = m + x
                  g x y

              buf.[0] <- f 1 7
      @>
      |> createTest "Renamer Test" "Renamer Test.cl"

      <@
          fun (range: Range1D) (buf: int clarray) ->
              let f x y = x - y
              buf.[0] <- f 2 3
              buf.[1] <- f 4 5
      @>
      |> createTest "Nested functions" "Nested.Function.cl" ]

let tests = lambdaLiftingTests |> testList "LambdaLifting"
