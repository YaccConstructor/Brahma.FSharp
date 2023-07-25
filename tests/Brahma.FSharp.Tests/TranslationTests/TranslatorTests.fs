module TranslatorTests

open Expecto
open Brahma.FSharp
open Brahma.FSharp.Tests
open System.IO
open Brahma.FSharp.OpenCL.Printer
open Brahma.FSharp.OpenCL.Translator
open FSharp.Quotations

[<AutoOpen>]
module Helpers =
    let basePath = "TranslationTests/Expected/"
    let generatedPath = "TranslationTests/Generated/"

    do Directory.CreateDirectory(generatedPath) |> ignore

    let openclTranslate (translator: FSQuotationToOpenCLTranslator) (expr: Expr) =
        let (ast, _) = translator.Translate expr
        AST.print ast

    let checkCode translator command outFile expected =
        let code = command |> openclTranslate translator

        let targetPath = Path.Combine(generatedPath, outFile)
        let expectedPath = Path.Combine(basePath, expected)
        File.WriteAllText(targetPath, code)

        Utils.filesAreEqual targetPath expectedPath

let basicLocalIdTests translator = [
    let inline checkCode cmd outFile expected = checkCode translator cmd outFile expected

    testCase "LocalID of 1D" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let id = range.LocalID0
                buf.[id] <- 0
            @>

        checkCode command "LocalID1D.gen" "LocalID1D.cl"

    testCase "LocalID of 2D" <| fun _ ->
        let command =
            <@ fun (range: Range2D) (buf: int clarray) ->
                let v = range.LocalID0
                let id = range.LocalID1
                buf.[id] <- v
            @>

        checkCode command "LocalID2D.gen" "LocalID2D.cl"
]

let basicWorkSizeTests translator = [
    let inline checkCode cmd outFile expected = checkCode translator cmd outFile expected

    testCase "WorkSize of 1D" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf: int clarray) ->
                    let gSize = range.GlobalWorkSize
                    let lSize = range.LocalWorkSize
                    ()
            @>

        checkCode command "WorkSize1D.gen" "WorkSize1D.cl"

    testCase "WorkSize of 2D" <| fun _ ->
        let command =
            <@
                fun (range: Range2D) (buf: int clarray) ->
                    let (gSizeX, gSizeY) = range.GlobalWorkSize
                    let (lSizeX, lSizeY) = range.LocalWorkSize
                    ()
            @>

        checkCode command "WorkSize2D.gen" "WorkSize2D.cl"

    testCase "WorkSize of 3D" <| fun _ ->
        let command =
           <@
                fun (range: Range3D) (buf: int clarray) ->
                    let (gSizeX, gSizeY, gSizeZ) = range.GlobalWorkSize
                    let (lSizeX, lSizeY, lSizeZ) = range.LocalWorkSize
                    ()
            @>

        checkCode command "WorkSize3D.gen" "WorkSize3D.cl"
]

let basicBinOpsTests translator = [
    let inline checkCode cmd outFile expected = checkCode translator cmd outFile expected

    testCase "Array item set" <| fun _ ->
        let command = <@ fun (range: Range1D) (buf: int clarray) -> buf.[0] <- 0 @>

        checkCode command "Array.Item.Set.gen" "Array.Item.Set.cl"

    testCase "Binding" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let x = 1
                buf.[0] <- x
            @>

        checkCode command "Binding.gen" "Binding.cl"

    testCase "Binop plus" <| fun _ ->
        let command = <@ fun (range: Range1D) (buf: int clarray) -> buf.[0] <- 1 + 2 @>

        checkCode command "Binop.Plus.gen" "Binop.Plus.cl"

    testCase "Binary operations. Math." <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let x = 0
                let y = x + 1
                let z = y * 2
                let a = z - x
                let i = a / 2
                buf.[0] <- i
            @>

        checkCode command "Binary.Operations.Math.gen" "Binary.Operations.Math.cl"

    testCase "TempVar from MAX transformation should not affect other variables" <| fun () ->
        let command =
            <@
                fun (range: Range1D) (buf: float clarray) ->
                    let tempVarY = 1.
                    buf.[0] <- max buf.[0] tempVarY
                    buf.[0] <- max buf.[0] tempVarY
            @>

        checkCode command "MAX.Transformation.gen" "MAX.Transformation.cl"
]

let controlFlowTests translator = [
    let inline checkCode cmd outFile expected = checkCode translator cmd outFile expected

    testCase "If Then" <| fun _ ->
        let command = <@ fun (range: Range1D) (buf: int clarray) -> if 0 = 2 then buf.[0] <- 1 @>

        checkCode command "If.Then.gen" "If.Then.cl"

    testCase "If Then Else" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                if 0 = 2 then
                    buf.[0] <- 1
                else
                    buf.[0] <- 2
            @>

        checkCode command "If.Then.Else.gen" "If.Then.Else.cl"

    testCase "For Integer Loop" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                for i in 1 .. 3 do
                    buf.[0] <- i
            @>

        checkCode command "For.Integer.Loop.gen" "For.Integer.Loop.cl"

    testCase "Sequential bindings" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let x = 1
                let y = x + 1
                buf.[0] <- y
            @>

        checkCode command "Sequential.Bindings.gen" "Sequential.Bindings.cl"

    testCase "Binding in IF." <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                if 2 = 0 then
                    let x = 1
                    buf.[0] <- x
                else
                    let i = 2
                    buf.[0] <- i
            @>

        checkCode command "Binding.In.IF.gen" "Binding.In.IF.cl"

    testCase "Binding in FOR." <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                for i in 0 .. 3 do
                    let x = i * i
                    buf.[0] <- x
            @>

        checkCode command "Binding.In.FOR.gen" "Binding.In.FOR.cl"

    testCase "Simple WHILE loop." <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                while buf.[0] < 5 do
                    buf.[0] <- buf.[0] + 1
            @>

        checkCode command "Simple.WHILE.gen" "Simple.WHILE.cl"

    testCase "Binding in WHILE." <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                while buf.[0] < 5 do
                    let x = buf.[0] + 1
                    buf.[0] <- x * x
            @>

        checkCode command "Binding.In.WHILE.gen" "Binding.In.WHILE.cl"

    ptestCase
        "WHILE with single statement in the body and this stetement is assignment of constant. \
        This test translates to openCL correctly but breaks openCL compiler on ubuntu 18.04" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                while true do
                    buf.[0] <- 1
            @>

        checkCode command "WHILE.with.complex.condition.gen" "WHILE.with.complex.condition.cl"

    testCase "WHILE with complex condition" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                while buf.[0] < 5 && (buf.[1] < 6 || buf.[2] > 2) do
                    buf.[0] <- 2 + buf.[0]
            @>

        checkCode command "WHILE.with.complex.condition.gen" "WHILE.with.complex.condition.cl"

    testCase "Simple seq." <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                buf.[0] <- 2
                buf.[1] <- 3
            @>

        checkCode command "Simple.Seq.gen" "Simple.Seq.cl"

    testCase "Seq with bindings." <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let x = 2
                buf.[0] <- x
                let y = 2
                buf.[1] <- y
            @>

        checkCode command "Seq.With.Bindings.gen" "Seq.With.Bindings.cl"
]

let namesResolvingTests translator = [
    let inline checkCode cmd outFile expected = checkCode translator cmd outFile expected

    testCase "Bindings with equal names." <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let x = 2
                buf.[0] <- x
                let x = 3
                buf.[1] <- x
            @>

        checkCode command "Bindings.With.Equal.Names.gen" "Bindings.With.Equal.Names.cl"

    testCase "Binding and FOR counter conflict 1." <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let i = 2

                for i in 1 .. 2 do
                    buf.[1] <- i
            @>

        checkCode command "Binding.And.FOR.Counter.Conflict.1.gen" "Binding.And.FOR.Counter.Conflict.1.cl"

    testCase "Binding and FOR counter conflict 2." <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                for i in 1 .. 2 do
                    let i = 2
                    buf.[1] <- i
            @>

        checkCode command "Binding.And.FOR.Counter.Conflict.2.gen" "Binding.And.FOR.Counter.Conflict.2.cl"

    testCase "Binding and FOR counter conflict 3." <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                for i in 0 .. 1 do
                    let i = i + 2
                    buf.[i] <- 2
            @>

        checkCode command "Binding.And.FOR.Counter.Conflict.3.gen" "Binding.And.FOR.Counter.Conflict.3.cl"

    testCase "Binding and FOR counter conflict 4." <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let i = 1

                for i in 0 .. i + 1 do
                    let i = i + 2
                    buf.[i] <- 2
            @>

        checkCode command "Binding.And.FOR.Counter.Conflict.4.gen" "Binding.And.FOR.Counter.Conflict.4.cl"
]

let quotationsInjectionTests translator = [
    let inline checkCode cmd outFile expected = checkCode translator cmd outFile expected

    testCase "Quotations injections 1" <| fun _ ->
        let myF = <@ fun x -> x * x @>

        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                buf.[0] <- (%myF) 2
                buf.[1] <- (%myF) 4
            @>

        checkCode command "Quotations.Injections.1.gen" "Quotations.Injections.1.cl"

    testCase "Quotations injections 2" <| fun _ ->
        let myF = <@ fun x y -> x - y @>

        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                buf.[0] <- (%myF) 2 3
                buf.[1] <- (%myF) 4 5
            @>

        checkCode command "Quotations.Injections.2.gen" "Quotations.Injections.2.cl"

]

let constantArrayTests translator = [
    let inline checkCode cmd outFile expected = checkCode translator cmd outFile expected

    testCase "Constant array translation. Test 1" <| fun _ ->
        let cArray1 = [| 1; 2; 3 |]
        let command = <@ fun (range: Range1D) (buf: int clarray) -> buf.[0] <- cArray1.[1] @>
        checkCode command "Constant array translation. Test 1.gen" "Constant array translation. Test 1.cl"

    testCase "Constant array translation. Test 2" <| fun _ ->
        let cArray1 = [| 1; 2; 3 |]
        let command = <@ fun (range: Range1D) (buf: int clarray) -> buf.[0] <- 1 + cArray1.[1] @>
        checkCode command "Constant array translation. Test 2.gen" "Constant array translation. Test 2.cl"
]

let lambdaLiftingTests translator = [
    let inline checkCode cmd outFile expected = checkCode translator cmd outFile expected

    testCase "Template Let Transformation Test 0" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f = 3
                buf.[0] <- f
            @>

        checkCode command "Template Test 0.gen" "Template Test 0.cl"

    testCase "Template Let Transformation Test 1" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f =
                    let x = 3
                    x

                buf.[0] <- f
            @>

        checkCode command "Template Test 1.gen" "Template Test 1.cl"

    testCase "Template Let Transformation Test 2" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f =
                    let x =
                        let y = 3
                        y

                    x

                buf.[0] <- f
            @>

        checkCode command "Template Test 2.gen" "Template Test 2.cl"

    testCase "Template Let Transformation Test 3" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f =
                    let f = 5
                    f

                buf.[0] <- f
            @>

        checkCode command "Template Test 3.gen" "Template Test 3.cl"

    testCase "Template Let Transformation Test 4" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f =
                    let f =
                        let f = 5
                        f

                    f

                buf.[0] <- f
            @>

        checkCode command "Template Test 4.gen" "Template Test 4.cl"

    testCase "Template Let Transformation Test 5" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f a b =
                    let x y z = y + z
                    x a b

                buf.[0] <- f 1 7
            @>

        checkCode command "Template Test 5.gen" "Template Test 5.cl"

    testCase "Template Let Transformation Test 6" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f x y =
                    let x = x
                    x + y

                buf.[0] <- f 7 8
            @>

        checkCode command "Template Test 6.gen" "Template Test 6.cl"

    testCase "Template Let Transformation Test 7" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f y =
                    let x y = 6 - y
                    x y

                buf.[0] <- f 7
            @>

        checkCode command "Template Test 7.gen" "Template Test 7.cl"

    testCase "Template Let Transformation Test 8" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (m: int clarray) ->
                let p = m.[0]

                let x n =
                    let l = m.[9]
                    let g k = k + m.[0] + m.[1]

                    let r =
                        let y a =
                            let x = 5 - n + (g 4)
                            let z t = m.[2] + a - t
                            z (a + x + l)

                        y 6

                    r + m.[3]

                m.[0] <- x 7
            @>

        checkCode command "Template Test 8.gen" "Template Test 8.cl"

    testCase "Template Let Transformation Test 9" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let x n =
                    let r = 8
                    let h = r + n
                    h

                buf.[0] <- x 9
            @>

        checkCode command "Template Test 9.gen" "Template Test 9.cl"

    testCase "Template Let Transformation Test 10" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let p = 9

                let x n b =
                    let t = 0
                    n + b + t

                buf.[0] <- x 7 9
            @>

        checkCode command "Template Test 10.gen" "Template Test 10.cl"

    testCase "Template Let Transformation Test 11" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let p = 1

                let m =
                    let r l = l + p
                    r 9

                let z k = k + 1
                buf.[0] <- m
            @>

        checkCode command "Template Test 11.gen" "Template Test 11.cl"

    testCase "Template Let Transformation Test 12" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f x y =
                    let y = y
                    let y = y
                    let g x m = m + x
                    g x y

                buf.[0] <- f 1 7
            @>

        checkCode command "Template Test 12.gen" "Template Test 12.cl"

    testCase "Template Let Transformation Test 13" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f y =
                    let y = y
                    let y = y
                    let g m = m + 1
                    g y

                buf.[0] <- f 7
            @>

        checkCode command "Template Test 13.gen" "Template Test 13.cl"

    testCase "Template Let Transformation Test 14" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f (y: int) =
                    let y = y
                    let y = y

                    let g (m: int) =
                        let g r t = r + y - t
                        let n o = o - (g y 2)
                        n 5

                    g y

                let z y = y - 2
                buf.[0] <- f (z 7)
            @>

        checkCode command "Template Test 14.gen" "Template Test 14.cl"

    testCase "Template Let Transformation Test 15" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f y =
                    let Argi index = if index = 0 then buf.[1] else buf.[2]
                    Argi y

                buf.[0] <- f 0
            @>

        checkCode command "Template Test 15.gen" "Template Test 15.cl"

    testCase "Template Let Transformation Test 16" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f y =
                    if y = 0 then
                        let z a = a + 1
                        z 9
                    else
                        buf.[2]

                buf.[0] <- f 0
            @>

        checkCode command "Template Test 16.gen" "Template Test 16.cl"

    testCase "Let renamed" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f x =
                    let g = 1 + x
                    g

                buf.[0] <- f 1
            @>

        checkCode command "Let renamed.gen" "Let renamed.cl"

    testCase "Let renamed 2" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f m k =
                    let g q w = 1 + q + w
                    let t p = 7 - p
                    (g 1 2) - m * k / (t 53)

                buf.[0] <- f 1 4
            @>

        checkCode command "Let renamed 2.gen" "Let renamed 2.cl"

    testCase "Renamer Test" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f x y =
                    let y = y
                    let y = y
                    let g x m = m + x
                    g x y

                buf.[0] <- f 1 7
            @>

        checkCode command "Renamer Test.gen" "Renamer Test.cl"

    testCase "Nested functions" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f x y = x - y
                buf.[0] <- f 2 3
                buf.[1] <- f 4 5
            @>

        checkCode command "Nested.Function.gen" "Nested.Function.cl"
]

let curryingTests translator = [
    let inline checkCode cmd outFile expected = checkCode translator cmd outFile expected

    testCase "Nested functions.Carring 1." <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f x y = x - y
                let g = f 2
                buf.[0] <- g 3
                buf.[1] <- g 5
            @>

        checkCode command "Nested.Function.Carring.gen" "Nested.Function.Carring.cl"

    testCase "Nested functions.Currying 2." <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f x y =
                    let gg = ref 0

                    for i in 1 .. x do
                        gg := !gg + y

                    !gg

                let g x = f 2 x
                buf.[0] <- g 2
                buf.[1] <- g 3
            @>

        checkCode command "Nested.Function.Carring2.gen" "Nested.Function.Carring2.cl"
]

let localMemoryTests translator = [
    let inline checkCode cmd outFile expected = checkCode translator cmd outFile expected

    testCase "Local int" <| fun _ ->
        let command =
            <@ fun (range: Range1D) ->
                let mutable x = local ()
                x <- 0
            @>

        checkCode command "LocalMemory.int.gen" "LocalMemory.int.cl"

    testCase "Local float" <| fun _ ->
        let command =
            <@ fun (range: Range1D) ->
                let mutable x = local ()
                x <- 0.0
            @>

        checkCode command "LocalMemory.float.gen" "LocalMemory.float.cl"

    testCase "Local int array" <| fun _ ->
        let command =
            <@ fun (range: Range1D) ->
                let xs = localArray 5
                xs.[range.LocalID0] <- range.LocalID0
            @>

        checkCode command "LocalMemory.int [].gen" "LocalMemory.int [].cl"
]

let localMemoryAllocationTests translator = [
    let inline checkCode cmd outFile expected = checkCode translator cmd outFile expected

    testCase "Constant array translation. Local copy test 1" <| fun _ ->
        let cArray1 = [| 1; 2; 3 |]

        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                //let c = local (Array.zeroCreate 3)//cArray1
                //buf.[0] <- c.[1]
                buf.[0] <- 1
            @>

        checkCode
            command
            "Constant array translation. Local copy test 1.gen"
            "Constant array translation. Local copy test 1.cl"
]

let printfTests translator = [
    let inline checkCode cmd outFile expected = checkCode translator cmd outFile expected

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

let barrierTests translator = [
    let inline checkCode cmd outFile expected = checkCode translator cmd outFile expected

    testCase "Local barrier translation tests" <| fun () ->
        let command = <@ fun (range: Range1D) -> barrierLocal () @>
        checkCode command "Barrier.Local.gen" "Barrier.Local.cl"

    testCase "Global barrier translation tests" <| fun () ->
        let command = <@ fun (range: Range1D) -> barrierGlobal () @>
        checkCode command "Barrier.Global.gen" "Barrier.Global.cl"

    testCase "Full barrier translation tests" <| fun () ->
        let command = <@ fun (range: Range1D) -> barrierFull () @>
        checkCode command "Barrier.Full.gen" "Barrier.Full.cl"
]

type TranslateTest =
    | A of int * float
    | B of double
    | C

open Brahma.FSharp.OpenCL.AST

let unionTests (translator: FSQuotationToOpenCLTranslator) =
    let testGen testCase name (types: List<System.Type>) outFile expectedFile =
        testCase name <| fun () ->
            let context = TranslationContext.Create(TranslatorOptions())
            for type' in types do Type.translateUnion type' |> State.run context |> ignore

            let unions = context.CStructDecls.Values |> Seq.map StructDecl |> Seq.toList

            let ast = AST <| List.map (fun du -> du :> ITopDef<_>) unions
            let code = AST.print ast

            File.WriteAllText(outFile, code)

            Utils.filesAreEqual outFile
            <| Path.Combine(basePath, expectedFile)

    [
        testGen testCase "Test 1" [ typeof<TranslateTest> ] "Translation.Test1.gen" "Translation.Test1.cl"
    ]

type SimpleUnion =
    | SimpleOne
    | SimpleTwo of int

type OuterUnion =
    | Outer of int
    | Inner of SimpleUnion

let collectUnionTests (translator: FSQuotationToOpenCLTranslator) =
    let testGen testCase name expected command =
        testCase name <| fun () ->
            let unions =
                Body.translate command
                |> State.exec (TranslationContext.Create(TranslatorOptions()))
                |> fun context -> context.CStructDecls.Keys

            Expect.sequenceEqual unions expected "Should be equal"

    [
        testGen testCase "Simple union" [| typeof<SimpleUnion> |]
            <@ let x = SimpleOne
               let y = SimpleTwo 2
               ()
            @>

        testGen testCase "Nested union 1" [| typeof<SimpleUnion>; typeof<OuterUnion> |]
            <@ let x = Outer 5
               ()
            @>

        testGen testCase "Nested union 2" [| typeof<SimpleUnion>; typeof<OuterUnion> |]
            <@ let x = Inner SimpleOne
               ()
            @>
    ]

let specificTests (translator: FSQuotationToOpenCLTranslator) = [
    let inline checkCode cmd outFile expected = checkCode translator cmd outFile expected

    testCase "Merge kernel" <| fun () ->
        let command workGroupSize =
            <@
                fun (ndRange: Range1D)
                    firstSide
                    secondSide
                    sumOfSides
                    (firstRowsBuffer: ClArray<int>)
                    (firstColumnsBuffer: ClArray<int>)
                    (firstValuesBuffer: ClArray<int>)
                    (secondRowsBuffer: ClArray<int>)
                    (secondColumnsBuffer: ClArray<int>)
                    (secondValuesBuffer: ClArray<int>)
                    (allRowsBuffer: ClArray<int>)
                    (allColumnsBuffer: ClArray<int>)
                    (allValuesBuffer: ClArray<int>) ->

                    let i = ndRange.GlobalID0

                    let mutable beginIdxLocal = local ()
                    let mutable endIdxLocal = local ()
                    let localID = ndRange.LocalID0

                    if localID < 2 then
                        let mutable x = localID * (workGroupSize - 1) + i - 1

                        if x >= sumOfSides then
                            x <- sumOfSides - 1

                        let diagonalNumber = x

                        let mutable leftEdge = diagonalNumber + 1 - secondSide
                        if leftEdge < 0 then leftEdge <- 0

                        let mutable rightEdge = firstSide - 1

                        if rightEdge > diagonalNumber then
                            rightEdge <- diagonalNumber

                        while leftEdge <= rightEdge do
                            let middleIdx = (leftEdge + rightEdge) / 2

                            let firstIndex: uint64 =
                                ((uint64 firstRowsBuffer.[middleIdx]) <<< 32)
                                ||| (uint64 firstColumnsBuffer.[middleIdx])

                            let secondIndex: uint64 =
                                ((uint64 secondRowsBuffer.[diagonalNumber - middleIdx])
                                 <<< 32)
                                ||| (uint64 secondColumnsBuffer.[diagonalNumber - middleIdx])

                            if firstIndex < secondIndex then
                                leftEdge <- middleIdx + 1
                            else
                                rightEdge <- middleIdx - 1

                        // Here localID equals either 0 or 1
                        if localID = 0 then
                            beginIdxLocal <- leftEdge
                        else
                            endIdxLocal <- leftEdge

                    barrierLocal ()

                    let beginIdx = beginIdxLocal
                    let endIdx = endIdxLocal
                    let firstLocalLength = endIdx - beginIdx
                    let mutable x = workGroupSize - firstLocalLength

                    if endIdx = firstSide then
                        x <- secondSide - i + localID + beginIdx

                    let secondLocalLength = x

                    //First indices are from 0 to firstLocalLength - 1 inclusive
                    //Second indices are from firstLocalLength to firstLocalLength + secondLocalLength - 1 inclusive
                    let localIndices = localArray<uint64> workGroupSize

                    if localID < firstLocalLength then
                        localIndices.[localID] <-
                            ((uint64 firstRowsBuffer.[beginIdx + localID])
                             <<< 32)
                            ||| (uint64 firstColumnsBuffer.[beginIdx + localID])

                    if localID < secondLocalLength then
                        localIndices.[firstLocalLength + localID] <-
                            ((uint64 secondRowsBuffer.[i - beginIdx]) <<< 32)
                            ||| (uint64 secondColumnsBuffer.[i - beginIdx])

                    barrierLocal ()

                    if i < sumOfSides then
                        let mutable leftEdge = localID + 1 - secondLocalLength
                        if leftEdge < 0 then leftEdge <- 0

                        let mutable rightEdge = firstLocalLength - 1

                        if rightEdge > localID then
                            rightEdge <- localID

                        while leftEdge <= rightEdge do
                            let middleIdx = (leftEdge + rightEdge) / 2
                            let firstIndex = localIndices.[middleIdx]

                            let secondIndex =
                                localIndices.[firstLocalLength + localID - middleIdx]

                            if firstIndex < secondIndex then
                                leftEdge <- middleIdx + 1
                            else
                                rightEdge <- middleIdx - 1

                        let boundaryX = rightEdge
                        let boundaryY = localID - leftEdge

                        // boundaryX and boundaryY can't be off the right edge of array (only off the left edge)
                        let isValidX = boundaryX >= 0
                        let isValidY = boundaryY >= 0

                        let mutable fstIdx = 0UL

                        if isValidX then
                            fstIdx <- localIndices.[boundaryX]

                        let mutable sndIdx = 0UL

                        if isValidY then
                            sndIdx <- localIndices.[firstLocalLength + boundaryY]

                        if not isValidX || isValidY && fstIdx < sndIdx then
                            allRowsBuffer.[i] <- int (sndIdx >>> 32)
                            allColumnsBuffer.[i] <- int sndIdx
                            allValuesBuffer.[i] <- secondValuesBuffer.[i - localID - beginIdx + boundaryY]
                        else
                            allRowsBuffer.[i] <- int (fstIdx >>> 32)
                            allColumnsBuffer.[i] <- int fstIdx
                            allValuesBuffer.[i] <- firstValuesBuffer.[beginIdx + boundaryX]
            @>

        checkCode (command 256) "MergeKernel.gen" "MergeKernel.cl"

    testCase "Multiple local values in atomic operations" <|  fun () ->
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

        openclTranslate translator kernel |> ignore
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

        command |> openclTranslate translator |> ignore

    // TODO is it correct?
    ptestCase "Using atomic in lambda should raise exception if first parameter is argument" <| fun () ->
        let command =
            <@
                fun (range:  Range1D) (buffer: int[]) ->
                let g x y = atomic (+) x y
                g buffer.[0] 6 |> ignore
            @>

        Expect.throwsT<System.ArgumentException>
        <| fun () -> command |> openclTranslate translator |> ignore
        <| "Exception should be thrown"
]

let tests translator =
    [
        testList "Basic tests on LocalID translation" << basicLocalIdTests
        testList "Basic tests on getting WorkSize translation" << basicWorkSizeTests
        testList "Basic operations translation tests" << basicBinOpsTests
        testList "Control flow translation tests" << controlFlowTests
        testList "Tests on variables renaming." << namesResolvingTests
        testList "Quotations injection tests" << quotationsInjectionTests
        testList "Constant array translation tests." << constantArrayTests
        testList "Let transformation tests" << lambdaLiftingTests
        ptestList "Currying translation test" << curryingTests
        testList "Test of local memory declaration functions" << localMemoryTests
        ptestList "Translation of local memory allocation functions" << localMemoryAllocationTests
        testList "Translation of printf" << printfTests
        testList "Barrier translation tests" << barrierTests
        testList "Translate union" << unionTests
        testList "Collect union tests" << collectUnionTests
        testList "Test on specific cases" << specificTests
        testList "Common Api Tests" << commonApiTests
    ]
    |> List.map (fun testFixture -> testFixture translator)

