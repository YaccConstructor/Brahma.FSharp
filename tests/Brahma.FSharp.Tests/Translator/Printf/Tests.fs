module Brahma.FSharp.Tests.Translator.Printf.Tests

open Expecto
open Brahma.FSharp
open Brahma.FSharp.Tests.Translator.Common

let printfTests translator = [
    let inline checkCode cmd outFile expected = Helpers.checkCode translator cmd outFile expected

    testCase "Printf test 1" <| fun _ ->
        let command = <@ fun (range: Range1D) -> printf "%d %f" 10 15.0 @>
        checkCode command "Printf test 1.gen" "Printf test 1.cl"

    testCase "Printf test 2" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (xs: int clarray) ->
                let gid = range.GlobalID0
                let x = 10

                printf "%d %d" x xs.[gid]
            @>

        checkCode command "Printf test 2.gen" "Printf test 2.cl"

    testCase "Printf test 3" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (xs: int clarray) ->
                let mutable i = 0

                while i < 10 do
                    xs.[0] <- i * 2
                    printf "i = %d, xs.[0]*10 = %d\n" i (xs.[0] + 10)
                    i <- i + 1
            @>

        checkCode command "Printf test 3.gen" "Printf test 3.cl"

    testCase "Printf test 4: printfn" <| fun _ ->
        let command = <@ fun (range: Range1D) -> printfn "%d %f" 10 15.0 @>
        checkCode command "Printf test 4.gen" "Printf test 4.cl"

    testCase "Printf test 5: printf without args" <| fun _ ->
        let command = <@ fun (range: Range1D) -> printf "I am complied" @>
        checkCode command "Printf test 5.gen" "Printf test 5.cl"

    testCase "Printf test 6: printfn without args" <| fun _ ->
        let command = <@ fun (range: Range1D) -> printfn "I am complied too" @>
        checkCode command "Printf test 6.gen" "Printf test 6.cl"
]
