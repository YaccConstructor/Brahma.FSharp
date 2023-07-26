module Brahma.FSharp.Tests.Translator.QuatationTransformation.Lifting

open Expecto
open Brahma.FSharp.OpenCL.Translator.QuotationTransformers
open Common
open FSharp.Quotations

let parameterLiftingTests =
    let createTest name =
        createMapTestAndCompareAsStrings Lift.Parameters.lift name

    [ createTest
          "Test 1"
          <@
              let x = 1
              let addToX y = x + y
              addToX 2
          @>
          <@
              let x = 1
              let addToX x y = x + y
              addToX x 2
          @>

      createTest
          "Test 2"
          <@
              let x = 1
              let z = x

              let addToX y = // freeVars: [x, z]
                  x + y + z

              let f z1 = // freeVars: [], addToX freeVars: [x, z]
                  2 + addToX z1

              f 3
          @>
          <@
              let x = 1
              let z = x

              let addToX x z y = x + y + z
              let f x z z1 = 2 + addToX x z z1
              f x z 3
          @>

      createTest
          "Test 3"
          <@
              let mainX = "global variable"
              let mainY = "global variable"
              let mainZ = "global variable"

              let foo fooX =
                  let fooY = "local variable of foo"
                  let bar barX = mainX + fooY + barX
                  bar fooX + mainY

              foo mainZ
          @>
          <@
              let mainX = "global variable"
              let mainY = "global variable"
              let mainZ = "global variable"

              let foo mainX mainY fooX =
                  let fooY = "local variable of foo"
                  let bar fooY mainX barX = mainX + fooY + barX
                  bar fooY mainX fooX + mainY

              foo mainX mainY mainZ
          @>

      createTest
          "Test 4"
          <@
              let x0 = 0

              let f x1 =
                  let g x2 =
                      let h x3 = x3 + x0
                      h x2

                  g x1

              f x0
          @>
          <@
              let x0 = 0

              let f x0 x1 =
                  let g x0 x2 =
                      let h x0 x3 = x3 + x0
                      h x0 x2

                  g x0 x1

              f x0 x0
          @>

      createTest "Test 5" // id
      <| <@ let f = let x = 4 in x in () @>
      <| <@ let f = let x = 4 in x in () @> ]
    |> testList "Parameter"

let unitVar name = expVar<unit> name

let unitCleanUpTests =
    let createTest name =
        createMapTestAndCompareAsStrings Lift.UnitArguments.cleanUp name

    [ createTest "Test 1"
      <| <@ let f (x: unit) = x in () @>
      <| <@ let f (x: unit) = x in () @>

      createTest "Test 2"
      <| <@ let f (x: unit) (y: int) = x in () @>
      <| <@ let f (y: int) = (%unitVar "x") in () @>

      createTest "Test 3"
      <| <@ let f (x: unit) (y: unit) = x in () @>
      <| <@ let f (x: unit) = x in () @>

      createTest "Test 3.5"
      <| <@ let f (x: unit) (y: unit) = y in () @>
      <| <@ let f (x: unit) = (%unitVar "y") in () @>

      createTest "Test 4"
      <| <@ let f (x: int) = x in () @>
      <| <@ let f (x: int) = x in () @>

      createTest "Test 5"
      <| <@ let f (x: int option) = x in () @>
      <| <@ let f (x: int option) = x in () @>

      createTest "Test 6"
      <| <@ let f (x: unit option) = x in () @>
      <| <@ let f (x: unit option) = x in () @>

      createTest "Test 7"
      <| <@ let f (x: unit) (y: unit) (z: unit) = if x = y then z else y in () @>
      <| <@
          let f (x: unit) =
              if x = (%unitVar "y") then
                  (%unitVar "z")
              else
                  (%unitVar "y") in ()
      @>

      createTest "Test 8"
      <| <@ let f (x: unit) = let g (y: unit) = Some() in () in () @>
      <| <@ let f (x: unit) = let g (y: unit) = Some() in () in () @>

      createTest "Test 9"
      <| <@ let f (x: unit) (y: unit) = let g (z: unit) (c: unit) = x in g y x in () @>
      <| <@ let f (x: unit) = let g (z: unit) = x in g (%unitVar "y") in () @>

      createTest "Test 10"
      <| <@
          let f () =
              printfn "side effect"
              ()

          let g (x: unit) (y: unit) (z: int) = z

          // side effect in f application
          g (f ()) () 0
      @>
      <| <@
          let f () =
              printfn "side effect"
              ()

          let g (z: int) = z

          f () // side effect
          g 0
      @>

      createTest "Test 11"
      <| <@
          let f (x: int) =
              printfn "side effect"
              () in

          let g (x: unit) (y: int) = y in

          // side effect in f application
          g (f 0) 0
      @>
      <| <@
          let f (x: int) =
              printfn "side effect"
              () in

          let g (y: int) = y in

          f 0 // side effect
          g 0
      @>

      createTest "Test 12" // id
      <| <@ let f (x: int) = x in f 4 @>
      <| <@ let f (x: int) = x in f 4 @>

      createTest "Test 13"
      <| <@ let f = let fUnitFunc () = let x = 3 in x in fUnitFunc () in () @>
      <| <@ let f = let fUnitFunc () = let x = 3 in x in fUnitFunc () in () @> ]
    |> testList "Unit clean up"

let lambdaLiftingTests =
    let inline createTest name expr expectedKernel (expectedFunctions: (Var * #Expr) list) =
        test name {
            let actualKernel, actualFunctions = Lift.Lambda.lift expr

            typesEqual actualKernel expectedKernel

            (actualFunctions, expectedFunctions) ||> List.iter2 assertMethodEqual

            equalAsStrings actualKernel expectedKernel <| "Kernels should be the same"
        }

    [ createTest "Test 1"
      <| <@ let f () = () in () @> // source
      <| <@ () @> // kernel
      <| [ var<unit -> unit> "f", <@ fun (unitVar0: unit) -> () @> ] // lifted lambdas (var, body)

      createTest "Test 2"
      <| <@ let f () = printfn "text" in () @>
      <| <@ () @>
      <| [ var<unit -> unit> "f", <@ fun (unitVar0: unit) -> printfn "text" @> ]

      createTest "Test 3"
      <| <@ let f (x: int) = () in () @>
      <| <@ () @>
      <| [ var<int -> unit> "f", <@ fun (x: int) -> () @> ]

      createTest "Test 4"
      <| <@ let f (x: int) = Some 0 in () @>
      <| <@ () @>
      <| [ var<int -> int option> "f", <@ fun (x: int) -> Some 0 @> ]

      createTest "Test 5"
      <| <@
          let f () =
              printfn "first"
              printfn "second" in ()
      @>
      <| <@ () @>
      <| [ var<unit -> unit> "f",
           <@
               fun (unitVar0: unit) ->
                   printfn "first"
                   printfn "second"
           @> ]

      createTest "Test 6"
      <| <@
          let f () = () in
          let g () = () in
          ()
      @>
      <| <@ () @>
      <| [ var<unit -> unit> "f", <@ fun (unitVar0: unit) -> () @>
           var<unit -> unit> "g", <@ fun (unitVar0: unit) -> () @> ]

      createTest "Test 7"
      <| <@ let f () = let g () = () in () in () @>
      <| <@ () @>
      <| [ var<unit -> unit> "g", <@ fun (unitVar0: unit) -> () @>
           var<unit -> unit> "f", <@ fun (unitVar0: unit) -> () @> ]

      createTest "Test 8"
      <| <@ let f (x: int) = let g () = x in () in () @>
      <| <@ () @>
      <| [ var<unit -> int> "g", <@@ fun (unitVar0: unit) -> (%expVar<int> "x") @@>
           var<int -> unit> "f", <@@ fun (x: int) -> () @@> ] ]
    |> testList "Lambda"

let tests =
    [ parameterLiftingTests
      unitCleanUpTests
      lambdaLiftingTests ]
    |> testList "Lifting"
