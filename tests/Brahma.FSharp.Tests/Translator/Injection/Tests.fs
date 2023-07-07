module Brahma.FSharp.Tests.Translator.Injection

open Brahma.FSharp
open Brahma.FSharp.Tests.Translator.Common
open System.IO
open Expecto

let private basePath = Path.Combine("Translator", "Injection", "Expected")

let private quotationsInjectionTests =
    [ let inline createTest name = Helpers.createTest basePath name

      let myF = <@ fun x -> x * x @>

      <@
          fun (range: Range1D) (buf: int clarray) ->
              buf.[0] <- (%myF) 2
              buf.[1] <- (%myF) 4
      @>
      |> createTest "Quotations injections 1" "Quotations.Injections.1.cl"

      let myF = <@ fun x y -> x - y @>

      <@
          fun (range: Range1D) (buf: int clarray) ->
              buf.[0] <- (%myF) 2 3
              buf.[1] <- (%myF) 4 5
      @>
      |> createTest "Quotations injections 2" "Quotations.Injections.2.cl" ]

let tests = quotationsInjectionTests |> testList "QuotationsInjection"
