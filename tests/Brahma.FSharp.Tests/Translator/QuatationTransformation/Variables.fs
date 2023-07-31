module Brahma.FSharp.Tests.Translator.QuatationTransformation.Variables

open Brahma.FSharp.OpenCL.Translator.QuotationTransformers
open Expecto

let private uniquesTests =
    [
        let createTest name =
            Common.Helpers.createMapTestAndCompareAsStrings Variables.defsToLambda name

        createTest "Test 1." <| <@ let x = 1 + 1 in () @> <| <@ let x = 1 + 1 in () @>

        createTest "Test 2."
        <| <@
            let x =
                let mutable y = 0

                for i in 1..10 do
                    y <- y + i

                y

            x
        @>
        <| <@
            let x =
                let xUnitFunc () =
                    let mutable y = 0

                    for i in 1..10 do
                        y <- y + i

                    y

                xUnitFunc ()

            x
        @>

        createTest "Test 3."
        <| <@
            let x =
                let mutable y =
                    if true then
                        let z = 10
                        z + 1
                    else
                        let z = 20
                        z + 2

                for i in 1..10 do
                    let z = if false then 10 else 20
                    y <- y + i + z

                y

            x
        @>
        <| <@
            let x =
                let xUnitFunc () =
                    let mutable y =
                        let yUnitFunc () =
                            if true then
                                let z = 10
                                z + 1
                            else
                                let z = 20
                                z + 2

                        yUnitFunc ()

                    for i in 1..10 do
                        let z =
                            let zUnitFunc () = if false then 10 else 20
                            zUnitFunc ()

                        y <- y + i + z

                    y

                xUnitFunc ()

            x
        @>

        createTest "Test 4"
        <| <@ let f = let x = 4 in x in () @>
        <| <@ let f = let fUnitFunc () = let x = 4 in x in fUnitFunc () in () @>

        createTest "Test 5"
        <| <@ let f = let g = let x = 4 in x in () in () @>
        <| <@
            let f =
                let fUnitFunc () =
                    let g =
                        let gUnitFunc () = let x = 4 in x
                        gUnitFunc ()

                    ()

                fUnitFunc ()

            ()
        @>
    ]

let tests = uniquesTests |> testList "Variables" |> testSequenced
