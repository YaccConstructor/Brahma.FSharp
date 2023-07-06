module Brahma.FSharp.Tests.Translator.NamesResolving

open Brahma.FSharp
open Brahma.FSharp.Tests.Translator.Common
open System.IO
open Expecto

let private basePath = Path.Combine("Translator", "NamesResolving", "Expected")

let private namesResolvingTests translator =
    [ let inline createTest name =
          Helpers.createTest translator basePath name

      <@
          fun (range: Range1D) (buf: int clarray) ->
              let x = 2
              buf.[0] <- x
              let x = 3
              buf.[1] <- x
      @>
      |> createTest "Bindings with equal names." "Bindings.With.Equal.Names.cl"

      <@
          fun (range: Range1D) (buf: int clarray) ->
              let i = 2

              for i in 1..2 do
                  buf.[1] <- i
      @>
      |> createTest "Binding and FOR counter conflict 1." "Binding.And.FOR.Counter.Conflict.1.cl"

      <@
          fun (range: Range1D) (buf: int clarray) ->
              for i in 1..2 do
                  let i = 2
                  buf.[1] <- i
      @>
      |> createTest "Binding and FOR counter conflict 2." "Binding.And.FOR.Counter.Conflict.2.cl"

      <@
          fun (range: Range1D) (buf: int clarray) ->
              for i in 0..1 do
                  let i = i + 2
                  buf.[i] <- 2
      @>
      |> createTest "Binding and FOR counter conflict 3." "Binding.And.FOR.Counter.Conflict.3.cl"

      <@
          fun (range: Range1D) (buf: int clarray) ->
              let i = 1

              for i in 0 .. i + 1 do
                  let i = i + 2
                  buf.[i] <- 2
      @>
      |> createTest "Binding and FOR counter conflict 4." "Binding.And.FOR.Counter.Conflict.4.cl" ]

let tests translator =
    namesResolvingTests translator
    |> testList "NamesResolving"
