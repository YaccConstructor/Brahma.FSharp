module Brahma.FSharp.Tests.Translator.ControlFlow

open Brahma.FSharp
open Brahma.FSharp.Tests.Translator.Common
open System.IO
open Expecto
open Brahma.FSharp.OpenCL.Translator

let private basePath = Path.Combine("Translator", "ControlFlow", "Expected")

let private controlFlowTests =
    [
        let inline createTest name = Helpers.createTest basePath name

        let inline createPTest name _ = Helpers.createPTest name

        <@
            fun (range: Range1D) (buf: int clarray) ->
                if 0 = 2 then
                    buf.[0] <- 1
        @>
        |> createTest "If Then" "If.Then.cl"

        <@ fun (range: Range1D) (buf: int clarray) -> if 0 = 2 then buf.[0] <- 1 else buf.[0] <- 2 @>
        |> createTest "If Then Else" "If.Then.Else.cl"

        <@
            fun (range: Range1D) (buf: int clarray) ->
                for i in 1..3 do
                    buf.[0] <- i
        @>
        |> createTest "For Integer Loop" "For.Integer.Loop.cl"

        <@
            fun (range: Range1D) (buf: int clarray) ->
                let x = 1
                let y = x + 1
                buf.[0] <- y
        @>
        |> createTest "Sequential bindings" "Sequential.Bindings.cl"

        <@
            fun (range: Range1D) (buf: int clarray) ->
                if 2 = 0 then
                    let x = 1
                    buf.[0] <- x
                else
                    let i = 2
                    buf.[0] <- i
        @>
        |> createTest "Binding in IF." "Binding.In.IF.cl"

        <@
            fun (range: Range1D) (buf: int clarray) ->
                for i in 0..3 do
                    let x = i * i
                    buf.[0] <- x
        @>
        |> createTest "Binding in FOR." "Binding.In.FOR.cl"

        <@
            fun (range: Range1D) (buf: int clarray) ->
                while buf.[0] < 5 do
                    buf.[0] <- buf.[0] + 1
        @>
        |> createTest "Simple WHILE loop." "Simple.WHILE.cl"

        <@
            fun (range: Range1D) (buf: int clarray) ->
                while buf.[0] < 5 do
                    let x = buf.[0] + 1
                    buf.[0] <- x * x
        @>
        |> createTest "Binding in WHILE." "Binding.In.WHILE.cl"

        // WHILE with single statement in the body and this stetement is assignment of constant.
        // This test translates to openCL correctly but breaks openCL compiler on ubuntu 18.04
        <@
            fun (range: Range1D) (buf: int clarray) ->
                while true do
                    buf.[0] <- 1
        @>
        |> createPTest "WHILE with single statement."

        <@
            fun (range: Range1D) (buf: int clarray) ->
                while buf.[0] < 5 && (buf.[1] < 6 || buf.[2] > 2) do
                    buf.[0] <- 2 + buf.[0]
        @>
        |> createTest "WHILE with complex condition" "WHILE.with.complex.condition.cl"

        <@
            fun (range: Range1D) (buf: int clarray) ->
                buf.[0] <- 2
                buf.[1] <- 3
        @>
        |> createTest "Simple seq." "Simple.Seq.cl"

        <@
            fun (range: Range1D) (buf: int clarray) ->
                let x = 2
                buf.[0] <- x
                let y = 2
                buf.[1] <- y
        @>
        |> createTest "Seq with bindings." "Seq.With.Bindings.cl"
    ]

let tests = controlFlowTests |> testList "ControlFlow"
