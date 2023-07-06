module Brahma.FSharp.Tests.Translator.Carrying

open Brahma.FSharp
open Brahma.FSharp.Tests.Translator.Common
open System.IO
open Expecto

let private basePath = Path.Combine("Translator", "Carrying", "Expected")

let private curryingTests translator =
    [ let inline createTest name = Helpers.createTest translator basePath name

      <@
          fun (range: Range1D) (buf: int clarray) ->
              let f x y = x - y
              let g = f 2
              buf.[0] <- g 3
              buf.[1] <- g 5
      @>
      |> createTest "Nested functions.Carrying 1." "Nested.Function.Carring.cl"

      <@
          fun (range: Range1D) (buf: int clarray) ->
              let f x y =
                  let gg = ref 0

                  for i in 1..x do
                      gg := !gg + y

                  !gg

              let g x = f 2 x
              buf.[0] <- g 2
              buf.[1] <- g 3
      @>
      |> createTest "Nested functions.Currying 2." "Nested.Function.Carring2.cl" ]

let tests translator =
    curryingTests translator
    |> testList "Currying"

