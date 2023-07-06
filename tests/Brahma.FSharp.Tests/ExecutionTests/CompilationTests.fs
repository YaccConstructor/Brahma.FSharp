module CompilationTests

open Expecto
open Brahma.FSharp.Tests
open FSharp.Quotations
open System.IO
open Brahma.FSharp

[<AutoOpen>]
module Helpers =
    let basePath = "ExecutionTests/Expected/"
    let generatedPath = "ExecutionTests/Generated/"

    Directory.CreateDirectory(generatedPath) |> ignore

    let openclCompile (context: RuntimeContext) (command: Expr<'a -> 'b>) =
        let kernel = context.ClContext.Compile(command)
        kernel.Code

    let checkCode context command outFile expected =
        let code = openclCompile context command

        let targetPath = Path.Combine(generatedPath, outFile)
        let expectedPath = Path.Combine(basePath, expected)
        File.WriteAllText(targetPath, code)

        Utils.filesAreEqual targetPath expectedPath

let simpleTests context =
    [ let inline checkCode command outFile expected =
          checkCode context command outFile expected

      testCase "Pointers to private values should be explicitly private"
      <| fun () ->
          let command =
              <@
                  fun (k: Range1D) (a: int clarray) ->
                      let x (a: int) = a + 1

                      let mutable s = 1
                      let mutable s = 2
                      let s1 = x s

                      a.[0] <- s1
              @>

          checkCode command "GenericSpace.gen" "GenericSpace.cl" ]

type SimpleUnion =
    | SimpleOne
    | SimpleTwo of int

type OuterUnion =
    | Outer of int
    | Inner of SimpleUnion

type TranslateTest =
    | A of int * float
    | B of double
    | C

type TranslateMatchTestUnion =
    | Case1
    | Case2 of int
    | Case3 of int * int

let unionTests context =
    let testGen testCase name outFile expectedFile command =
        testCase name <| fun () -> checkCode context command outFile expectedFile

    let newUnionTestList =
        [ testGen
              testCase
              "Test 1: TranslateTest.A"
              "Union.Compile.Test1.gen"
              "Union.Compile.Test1.cl"
              <@
                  fun (range: Range1D) ->
                      let x = A(5, 6.0)
                      let mutable y = 5
                      y <- 7
              @>

          testGen
              ptestCase // TODO(https://github.com/YaccConstructor/Brahma.FSharp/issues/152)
              "Test 2: TranslateTest.B"
              "Union.Compile.Test2.gen"
              "Union.Compile.Test2.cl"
              <@
                  fun (range: Range1D) ->
                      let x = B(5.0)
                      let mutable y = 5
                      y <- 7
              @>

          testGen
              testCase
              "Test 3: TranslateTest.C"
              "Union.Compile.Test3.gen"
              "Union.Compile.Test3.cl"
              <@
                  fun (range: Range1D) ->
                      let x = C
                      let mutable y = 5
                      y <- 7
              @>

          testGen
              testCase
              "Test 4: OuterUnion.Outer"
              "Union.Compile.Test4.gen"
              "Union.Compile.Test4.cl"
              <@
                  fun (range: Range1D) ->
                      let x = Inner SimpleOne
                      let mutable y = 5
                      y <- 7
              @>

          testGen
              testCase
              "Test 5: OuterUnion.Inner"
              "Union.Compile.Test5.gen"
              "Union.Compile.Test5.cl"
              <@
                  fun (range: Range1D) ->
                      let x = Inner(SimpleTwo 29)
                      let mutable y = 5
                      y <- 7
              @> ]

    let testUnionCaseTestLists =
        [ testGen
              ptestCase // TODO(https://github.com/YaccConstructor/Brahma.FSharp/issues/152)
              "Test 1: simple pattern matching"
              "Union.Compile.Test6.gen"
              "Union.Compile.Test6.cl"
              <@
                  fun (range: Range1D) ->
                      let t = Case1
                      let mutable x = 5

                      match t with
                      | Case1 -> x <- 5
                      | Case2(_) -> x <- 6
                      | Case3(_) -> x <- 7
              @> ]

    let unionPropertyGetTestLists =
        [ testGen
              ptestCase // TODO(https://github.com/YaccConstructor/Brahma.FSharp/issues/152)
              "Test 1: simple pattern matching bindings"
              "Union.Compile.Test7.gen"
              "Union.Compile.Test7.cl"
              <@
                  fun (range: Range1D) ->
                      let t = Case1
                      let mutable m = 5

                      match t with
                      | Case1 -> m <- 5
                      | Case2(x) -> m <- x
                      | Case3(y, z) -> m <- y + z
              @> ]

    [ testList "NewUnion" newUnionTestList
      testList "TestUnionCase" testUnionCaseTestLists
      testList "UnionPropertyGet" unionPropertyGetTestLists ]

let tests context =
    [ testList "Simple tests" << simpleTests
      testList "Union Compile tests" << unionTests ]
    |> List.map (fun testFixture -> testFixture context)
