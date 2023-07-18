module Brahma.FSharp.Tests.Translator.QuatationTransformation.VarToRef

open System.Collections.Generic
open Brahma.FSharp.OpenCL.Translator.QuotationTransformers
open Expecto
open FSharp.Quotations

let private uniquesTests =
    [ let createTest name source expected =
          test name {
              let actual = VarToRef.transform source

              let actualStr = actual.ToString()
              let expectedStr = expected.ToString()

              Expect.equal actualStr expectedStr "Result should be the same."
          }

      createTest "Test 1" // id (no mutable vars)
      <| <@
          let firstVar = ()
          let secondVar = 2

          let f () =
              firstVar
              secondVar

          () @>
      <| <@
          let firstVar = ()
          let secondVar = 2

          let f () =
              firstVar
              secondVar

          () @>

      createTest "Test 2" // transform mutable var (unit type)
      <| <@ let mutable firstVar = ()
            let f (x: int) = firstVar
            () @>
      <| <@ let mutable firstVar = ()
            let firstVarRef = ref firstVar
            let f (x: int) = !firstVarRef // firstVar free in f TODO(_.Value)
            () @>

      createTest "Test 3"
      <| <@ let mutable firstVar = 1
            let f () = firstVar
            () @>
      <| <@ let mutable firstVar = 1
            let firstVarRef = ref firstVar
            let f () = !firstVarRef
            () @>

      createTest "Test 4"
      <| <@ let mutable firstVar = 1
            let f () = firstVar <- 1
            () @>
      <| <@ let mutable firstVar = 1
            let firstVarRef = ref firstVar
            let f () = firstVarRef := 1
            () @>

      createTest "Test 5"
      <| <@ let mutable firstVar = 1
            let f () =
                firstVar <- 2
                firstVar
            ()@>
      <| <@ let mutable firstVar = 1
            let firstVarRef = ref firstVar
            let f () =
                firstVarRef := 2
                !firstVarRef
            () @>

      createTest "Test 6"
      <| <@ let mutable firstVar = 1
            let mutable secondVar = 0.5

            let f () =
                firstVar <- 3
                secondVar <- 0.25

                ()
            () @>
      <| <@ let mutable firstVar = 1
            let firstVarRef = ref firstVar

            let secondVar = 0.5
            let secondVarRef = ref secondVar

            let f () =
                firstVarRef := 3
                secondVarRef := 0.25

                ()
            () @>

      createTest "Test 7" // id (mutable fun)
      <| <@ let mutable firstFun = fun () -> 1

            let f () =
                firstFun <- fun () -> 2

                ()
            () @>
      <| <@ let mutable firstFun = fun () -> 1

            let f () =
                firstFun <- fun () -> 2

                ()
            () @>

      createTest "Test 8"
      <| <@ let mutable firstVar =
                let mutable innerVar = None

                let f (x: int) = innerVar <- Some x
                Some 1
            () @>
      <| <@ let mutable firstVar =
                let mutable innerVar = None
                let innerVarRef = ref innerVar

                let f (x: int) = innerVarRef := Some x
                Some 1
            () @>

      createTest "Test 9"
      <| <@ let mutable firstVar =
                let mutable firstInnerVar =
                    let mutable secondInnerVar = Some 1

                    let f () = secondInnerVar

                    None

                let f () = firstInnerVar

                Some ()
            let f () = firstVar

            () @>
      <| <@ let mutable firstVar =
                let mutable firstInnerVar =
                    let mutable secondInnerVar = Some 1
                    let secondInnerVarRef = ref secondInnerVar

                    let f () = !secondInnerVarRef

                    None
                let firstInnerVarRef = ref firstInnerVar

                let f () = !firstInnerVarRef

                Some ()

            let firstVarRef = ref firstVar

            let f () = !firstVarRef

            () @>

      createTest "Test 10"
      <| <@ fun (x: int) (y: int option) ->
            let mutable firstVar = Some 2

            let f = fun () ->
                printfn ""
                firstVar <- None
                printfn ""
                firstVar <- Some 0
                firstVar

            () @>
      <| <@ fun (x: int) (y: int option) ->
            let mutable firstVar = Some 2
            let firstVarRef = ref firstVar

            let f = fun () ->
                printfn ""
                firstVarRef := None
                printfn ""
                firstVarRef := Some 0
                !firstVarRef
            () @>

      createTest "Test 11" // id
      <| <@ let mutable firstVar = ()
            let mutable secondVar = 2

            let firstVar = ()
            let f () = firstVar
            let g () = let secondVar = 2 in secondVar

            ()
         @>
      <| <@ let mutable firstVar = ()
            let mutable secondVar = 2

            let firstVar = ()
            let f () = firstVar
            let g () = let secondVar = 2 in secondVar

            () @> ]

let tests = uniquesTests |> testList "VarToRef" |> testSequenced
