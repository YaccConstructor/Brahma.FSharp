module Brahma.FSharp.Tests.Translator.QuatationTransformation.Names

open System.Collections.Generic
open Brahma.FSharp.OpenCL.Translator.QuotationTransformers
open Expecto
open FSharp.Quotations

// collect all vars and get them names
let private getNames expr =
    let dict = HashSet()

    let addToDict (var: Var) =
        if not <| dict.Contains var then
            dict.Add(var) |> ignore

    let rec get = function
        | ExprShape.ShapeVar var -> addToDict var
        | ExprShape.ShapeLambda(var, body) ->
            addToDict var
            get body
        | ExprShape.ShapeCombination(_, exprList) ->
            List.iter get exprList

    get expr

    dict
    |> Seq.map (fun var -> var.Name)

let private uniquesTests =
    [ let createTest name source =
          test name {
              let names =
                  Names.makeUnique source |> getNames

              let namesWithoutDuplicates = Seq.distinct names

              Expect.sequenceEqual names namesWithoutDuplicates "Result should be the same."
          }

      createTest "Test 1."
      <| <@
          fun var ->
              let var = ()
              let var = ()
              let var = ()

              ()
      @>

      createTest "Test 2."
      <| <@ fun f ->
               let f (x: int) = x
               let f (x: int) (y: int) = x

               let f = 4
               () @>

      createTest "Test 3."
      <| <@
          fun x y z z10 ->
              let mutable x = 4
              let mutable x = ()

              let y = 100

              let f (x: unit) (y: int) (z: int) = x

              let x = f x y 3

              let x = (fun (x: unit) -> fun (y: unit) -> fun (z: unit) -> y)

              let z = ()
              let y = ()
              let z10 = ()

              x z y z10

              ()
      @>

      createTest "Test 4."
      <| <@
          fun x1 y2 z3 z10 ->
              let mutable x3 = 4
              let mutable x1 = ()

              let y2 = 100

              let f (x: unit) (y: int) (z: int) = x

              let y3 = 3
              let x1 = f x1 y3 3

              let x = (fun (x4: unit) -> fun (y2: int) -> fun (z: unit) -> 2)

              let z124 = ()
              let y32 = ()
              let z10 = ()
              let z4 = ()

              let z11 = x z4 y2 z10

              ()
      @> ]

let tests = uniquesTests |> testList "Names" |> testSequenced
