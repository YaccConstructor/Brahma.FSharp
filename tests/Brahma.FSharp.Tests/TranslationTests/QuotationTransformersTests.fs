module QuotationTransformersTests

open Expecto
open FSharp.Quotations
open Brahma.FSharp.OpenCL.Translator
open Brahma.FSharp.OpenCL.Translator.QuotationTransformers
open Brahma.FSharp

[<AutoOpen>]
module Helpers =
    let eqMsg = "Values should be equal"

    let rec renameUnitVar (expr: Expr) =
        let replaceUnitVar (var: Var) =
            if var.Type = typeof<unit> then
                Var("unitVar", var.Type, var.IsMutable)
            else
                var

        match expr with
        | ExprShape.ShapeVar var -> Expr.Var(replaceUnitVar var)
        | ExprShape.ShapeLambda (var, body) -> Expr.Lambda(replaceUnitVar var, renameUnitVar body)
        | ExprShape.ShapeCombination (shapeComboObj, exprList) ->
            ExprShape.RebuildShapeCombination(shapeComboObj, List.map renameUnitVar exprList)

    let openclTransformQuotation (translator: FSQuotationToOpenCLTranslator) (expr: Expr) =
        translator.TransformQuotation expr

    let assertExprEqual (actual: Expr) (expected: Expr) (msg: string) =
        let actual' = renameUnitVar actual
        let expected' = renameUnitVar expected

        Expect.equal
        <| actual'.ToString()
        <| expected'.ToString()
        <| msg

    let assertMethodEqual (actual: Var * Expr) (expected: Var * Expr) =
        Expect.equal (fst actual).Name (fst expected).Name "Method names should be equal"

        assertExprEqual (snd actual) (snd expected)
        <| $"Method bodies of %s{(fst actual).Name} is not equal"
let lambdaLiftingTests =
    let genParameterLiftTest testCase name expr expected =
        testCase name <| fun _ ->
            let actual = LambdaLifting.parameterLiftExpr expr
            assertExprEqual actual expected eqMsg

    [
        genParameterLiftTest
            testCase
            "Test 1"
            <@ let x = 1
               let addToX y = x + y
               addToX 2
            @>
            <@ let x = 1
               let addToX x y = x + y
               addToX x 2
            @>

        genParameterLiftTest
            testCase
            "Test 2"
            <@ let x = 1
               let z = x

               let addToX y = // freeVars: [x, z]
                   x + y + z

               let f z1 = // freeVars: [], addToX freeVars: [x, z]
                   2 + addToX z1

               f 3
            @>
            <@ let x = 1
               let z = x

               let addToX x z y = x + y + z
               let f x z z1 = 2 + addToX x z z1
               f x z 3
            @>

        genParameterLiftTest
            testCase
            "Test 3"
            <@ let mainX = "global variable"
               let mainY = "global variable"
               let mainZ = "global variable"

               let foo fooX =
                   let fooY = "local variable of foo"
                   let bar barX = mainX + fooY + barX
                   bar fooX + mainY

               foo mainZ
            @>
            <@ let mainX = "global variable"
               let mainY = "global variable"
               let mainZ = "global variable"

               let foo mainX mainY fooX =
                   let fooY = "local variable of foo"
                   let bar fooY mainX barX = mainX + fooY + barX
                   bar fooY mainX fooX + mainY

               foo mainX mainY mainZ
            @>

        genParameterLiftTest
            testCase
            "Test 4"
            <@ let x0 = 0

               let f x1 =
                   let g x2 =
                       let h x3 = x3 + x0
                       h x2

                   g x1

               f x0
            @>
            <@ let x0 = 0

               let f x0 x1 =
                   let g x0 x2 =
                       let h x0 x3 = x3 + x0
                       h x0 x2

                   g x0 x1

               f x0 x0
            @>
    ]

let varDefsToLambdaTest =
    let genVarDefToLambdaTest testCase name expr expected =
        testCase name <| fun _ ->
            let actual = VarDefsToLambdaTransformer.transformVarDefsToLambda expr
            assertExprEqual actual expected eqMsg

    [
        genVarDefToLambdaTest
            testCase
            "Test 1"
            <@ let x =
                let mutable y = 0

                for i in 1 .. 10 do
                    y <- y + i

                y

               x
            @>
            <@ let x =
                let xUnitFunc () =
                    let mutable y = 0

                    for i in 1 .. 10 do
                        y <- y + i

                    y

                xUnitFunc ()

               x
            @>

        genVarDefToLambdaTest
            testCase
            "Test 2: we need to go deeper"
            <@ let x =
                let mutable y =
                    if true then
                        let z = 10
                        z + 1
                    else
                        let z = 20
                        z + 2

                for i in 1 .. 10 do
                    let z = if false then 10 else 20
                    y <- y + i + z

                y

               x
            @>
            <@ let x =
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

                    for i in 1 .. 10 do
                        let z =
                            let zUnitFunc () = if false then 10 else 20
                            zUnitFunc ()

                        y <- y + i + z

                    y

                xUnitFunc ()

               x
            @>
    ]

let quotationTransformerTest translator =
    let sprintfMethods (methods: seq<Method>) =
        Seq.map (fun (x: Method) -> $"%A{x.FunVar}\n%A{x.FunExpr}\n") methods
        |> String.concat "\n"

    let assertMethodListsEqual (actual: list<Var * Expr>) (expected: list<Var * Expr>) =
        Expect.equal actual.Length expected.Length "List sizes should be equal"

        List.zip actual expected
        |> List.iter (fun (x, y) -> assertMethodEqual x y)

    let makeMethods (expr: Expr) =
        let rec go (expr: Expr) =
            match expr with
            | Patterns.Let (var, body, inExpr) ->
                let methods, kernel = go inExpr
                (var, body) :: methods, kernel
            | _ -> [], expr

        let methods, kernelExpr = go expr
        kernelExpr, methods

    let genTest testCase name expr expected =
        let expectedKernelExpr, expectedMethods = makeMethods expected

        testCase name <| fun _ ->
            let (actualKernelExpr, actualKernelMethods) = expr |> openclTransformQuotation translator

            assertMethodListsEqual actualKernelMethods expectedMethods
            assertExprEqual actualKernelExpr expectedKernelExpr "kernels not equals"

    [
        genTest
            testCase
            "Test 0"
            <@ fun (range: Range1D) (buf: array<int>) ->
                let mutable x = 1
                let f y = x <- y
                f 10
                buf.[0] <- x
            @>
            <@
                let f xRef (y: int) = xRef := y

                fun (range: Range1D) (buf: array<int>) ->
                    let mutable x = 1
                    let xRef = ref x

                    f xRef 10
                    buf.[0] <- !xRef
            @>

        genTest
            testCase
            "Test 1"
            <@ fun (range: Range1D) (buf: array<int>) ->
                let mutable x = 1
                let f y = x <- x + y
                f 10
                buf.[0] <- x
            @>
            <@
                let f xRef (y: int) = xRef := !xRef + y

                fun (range: Range1D) (buf: array<int>) ->
                    let mutable x = 1
                    let xRef = ref x

                    f xRef 10
                    buf.[0] <- !xRef
            @>

        genTest
            testCase
            "Test 2: simple lambda lifting without capturing variables"
            <@ fun (range: Range1D) ->
                let f x =
                    let g y = y + 1
                    g x

                f 2 @>
            <@ let g y = y + 1
               let f x = g x
               fun (range: Range1D) -> f 2 @>

        genTest
            testCase
            "Test 3: simple lambda lifting with capturing variables"
            <@ fun (range: Range1D) ->
                let f x =
                    let g y = y + x
                    g (x + 1)

                f 2
            @>
            <@ let g x y = y + x
               let f x = g x (x + 1)
               fun (range: Range1D) -> f 2
            @>

        genTest
            testCase
            "Test 4"
            <@ fun (range: Range1D) (arr: array<int>) ->
                let x =
                    let mutable y = 0

                    let addToY x = y <- y + x

                    for i in 0 .. 10 do
                        addToY arr.[i]

                    y

                x
            @>
            <@ let addToY yRef x = yRef := !yRef + x

               let x1UnitFunc (arr: array<int>) =
                   let y = 0
                   let yRef = ref y

                   for i in 0 .. 10 do
                       addToY yRef arr.[i]

                   !yRef

               fun (range: Range1D) (arr: array<int>) ->
                   let x1 = x1UnitFunc arr
                   x1
            @>

        genTest
            testCase
            "Test 5"
            <@ fun (range: Range1D) (arr: array<int>) ->
                let mutable x = if 0 > 1 then 2 else 3

                let mutable y =
                    for i in 0 .. 10 do
                        x <- x + 1

                    x + 1

                let z = x + y

                let f () = arr.[0] <- x + y + z
                f ()
            @>
            <@ let xUnitFunc () = if 0 > 1 then 2 else 3

               let yUnitFunc xRef =
                   for i in 0 .. 10 do
                       xRef := !xRef + 1

                   !xRef + 1

               let f (arr: array<int>) xRef yRef z = arr.[0] <- !xRef + !yRef + z

               fun (range: Range1D) (arr: array<int>) ->
                   let mutable x = xUnitFunc ()
                   let xRef = ref x

                   let mutable y = yUnitFunc xRef
                   let yRef = ref y

                   let z = !xRef + !yRef

                   f arr xRef yRef z
            @>
    ]

let tests translator =
    [
        testList "Parameter lifting test" lambdaLiftingTests
        testList "Var defs to lambda test" varDefsToLambdaTest
        testList "Transformer quotation system tests" <| quotationTransformerTest translator
    ]
