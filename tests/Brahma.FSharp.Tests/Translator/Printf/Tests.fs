module Brahma.FSharp.Tests.Translator.Printf

open Brahma.FSharp
open Brahma.FSharp.Tests.Translator.Common
open System.IO
open Expecto

let private basePath = Path.Combine("Translator", "Printf", "Expected")

let private printfTests =
    [
        let inline createTest name = Helpers.createTest basePath name

        <@ fun (range: Range1D) -> printf "%d %f" 10 15.0 @>
        |> createTest "Printf test 1" "Printf test 1.cl"

        <@
            fun (range: Range1D) (xs: int clarray) ->
                let gid = range.GlobalID0
                let x = 10

                printf "%d %d" x xs.[gid]
        @>
        |> createTest "Printf test 2" "Printf test 2.cl"

        <@
            fun (range: Range1D) (xs: int clarray) ->
                let mutable i = 0

                while i < 10 do
                    xs.[0] <- i * 2
                    printf "i = %d, xs.[0]*10 = %d\n" i (xs.[0] + 10)
                    i <- i + 1
        @>
        |> createTest "Printf test 3" "Printf test 3.cl"

        <@ fun (range: Range1D) -> printfn "%d %f" 10 15.0 @>
        |> createTest "Printf test 4: printfn" "Printf test 4.cl"

        <@ fun (range: Range1D) -> printf "I am complied" @>
        |> createTest "Printf test 5: printf without args" "Printf test 5.cl"

        <@ fun (range: Range1D) -> printfn "I am complied too" @>
        |> createTest "Printf test 6: printfn without args" "Printf test 6.cl"
    ]

let tests = printfTests |> testList "Printf"
