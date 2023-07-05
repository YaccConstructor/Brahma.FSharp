module Brahma.FSharp.Tests.Translator.LangExtensions.Atomic

open Expecto
open Brahma.FSharp
open Brahma.FSharp.Tests.Translator.Common

let test translator=

    [ testCase "Multiple local values in atomic operations" <|  fun () ->
        let kernel =
            <@
                fun (ndRange: Range1D) (v: int) ->
                    let mutable firstMaxIndex = local ()
                    let mutable secondMaxIndex = local ()
                    let mutable value = local ()

                    if ndRange.LocalID0 = 0 then
                        firstMaxIndex <- 0
                        secondMaxIndex <- 0
                        value <- v

                    barrierLocal ()

                    atomic (max) firstMaxIndex value |> ignore
                    atomic (max) secondMaxIndex value |> ignore
            @>

        Helpers.openclTranslate translator kernel |> ignore
]

let commonApiTests translator = [
    // TODO is it correct?
    ptestCase "Using atomic in lambda should not raise exception if first parameter passed" <| fun () ->
        let command =
            <@
                fun (range:  Range1D) (buffer: int[]) ->
                let g = atomic (fun x y -> x + 1) buffer.[0]
                g 5 |> ignore
            @>

        command |> Helpers.openclTranslate translator |> ignore

    // TODO is it correct?
    ptestCase "Using atomic in lambda should raise exception if first parameter is argument" <| fun () ->
        let command =
            <@
                fun (range:  Range1D) (buffer: int[]) ->
                let g x y = atomic (+) x y
                g buffer.[0] 6 |> ignore
            @>

        Expect.throwsT<System.ArgumentException>
        <| fun () -> command |> Helpers.openclTranslate translator |> ignore
        <| "Exception should be thrown"
]
