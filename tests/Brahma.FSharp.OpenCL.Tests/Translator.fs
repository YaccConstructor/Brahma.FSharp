module Brahma.FSharp.OpenCL.Tests

open NUnit.Framework
open System.IO
//open Brahma.Helpers
//open OpenCL.Net
//open Brahma.OpenCL
//open Brahma.FSharp.OpenCL.Core
open System
open System.Reflection
//open Microsoft.FSharp.Quotations

open Brahma.Helpers
open OpenCL.Net
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open System
open System.Reflection
open Microsoft.FSharp.Quotations
open System.Runtime.InteropServices
open Brahma.FSharp.OpenCL.Extensions

[<Struct>]
type TestStruct =
    val x: int 
    val y: float
    new (x,y) = {x=x; y=y} 

[<TestFixture>]
type Translator() =
    
    let basePath = "../../../Brahma.FSharp.OpenCL/Translator/Expected/"

    let deviceType = DeviceType.Gpu
    let platformName = "NVIDIA*"

    let provider =
        try  ComputeProvider.Create(platformName, deviceType)
        with 
        | ex -> failwith ex.Message   

    let filesAreEqual file1 file2 =
        let all1 = File.ReadAllBytes file1
        let all2 = File.ReadAllBytes file2
        Assert.AreEqual (all1.Length, all2.Length)
        Assert.IsTrue(Array.forall2 (=) all1 all2)

    let checkCode command outFile expected =
        let code = ref ""
        let _ = provider.Compile(command,_outCode = code)
        printfn "\n%s" !code
        System.IO.File.WriteAllText(outFile,!code)
        filesAreEqual outFile (System.IO.Path.Combine(basePath,expected))

    let a = [|0..3|]

    [<Test>]
    member this.``Array item set``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) -> 
                    buf.[0] <- 0
            @>

        checkCode command "Array.Item.Set.gen" "Array.Item.Set.cl"

    [<Test>]
    member this.Binding() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) -> 
                    let x = 1
                    buf.[0] <- x
            @>

        checkCode command "Binding.gen" "Binding.cl"

    [<Test>]
    member this.``Binop plus``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) -> 
                    buf.[0] <- 1 + 2
            @>
        
        checkCode command "Binop.Plus.gen" "Binop.Plus.cl"

    [<Test>]
    member this.``If Then``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) -> 
                    if 0 = 2 then buf.[0] <- 1
            @>

        checkCode command "If.Then.gen" "If.Then.cl"

    [<Test>]
    member this.``If Then Else``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) -> 
                    if 0 = 2 then buf.[0] <- 1 else buf.[0] <- 2
            @>

        checkCode command "If.Then.Else.gen" "If.Then.Else.cl"

    [<Test>]
    member this.``For Integer Loop``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) -> 
                    for i in 1..3 do buf.[0] <- i
            @>

        checkCode command "For.Integer.Loop.gen" "For.Integer.Loop.cl"

    [<Test>]
    member this.``Sequential bindings``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) -> 
                    let x = 1
                    let y = x + 1
                    buf.[0] <- y
            @>

        checkCode command "Sequential.Bindings.gen" "Sequential.Bindings.cl"

    [<Test>]
    member this.``Binary operations. Math.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) -> 
                    let x = 0
                    let y = x + 1
                    let z = y * 2
                    let a = z - x
                    let i = a / 2
                    buf.[0] <- i
            @>

        checkCode command "Binary.Operations.Math.gen" "Binary.Operations.Math.cl"

    [<Test>]
    member this.``Binding in IF.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) -> 
                    if 2 = 0
                    then                        
                        let x = 1
                        buf.[0] <- x
                    else
                        let i = 2
                        buf.[0] <- i
            @>

        checkCode command "Binding.In.IF.gen" "Binding.In.IF.cl"

    [<Test>]
    member this.``Binding in FOR.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) -> 
                    for i in 0..3 do
                        let x = i * i
                        buf.[0] <- x
            @>

        checkCode command "Binding.In.FOR.gen" "Binding.In.FOR.cl"
       
    [<Test>]
    member this.``Simple WHILE loop.``() = 
        let command = 
            <@
                fun (range:_1D) (buf:array<int>) ->
                    while buf.[0] < 5 do
                        buf.[0] <- buf.[0] + 1
            @>

        checkCode command "Simple.WHILE.gen" "Simple.WHILE.cl"

    [<Test>]
    member this.``Binding in WHILE.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->
                 while buf.[0] < 5 do
                     let x = buf.[0] + 1
                     buf.[0] <- x * x
            @>

        checkCode command "Binding.In.WHILE.gen" "Binding.In.WHILE.cl"

    [<Test>]
    member this.``WHILE with complex condition.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->
                 while buf.[0] < 5 && (buf.[1] < 6 || buf.[2] > 2) do                     
                     buf.[0] <- 1
            @>

        checkCode command "WHILE.with.complex.condition.gen" "WHILE.with.complex.condition.cl"

    [<Test>]
    member this.``Simple seq.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->
                    buf.[0] <- 2
                    buf.[1] <- 3
            @>

        checkCode command "Simple.Seq.gen" "Simple.Seq.cl"

    //[<Test>]
    member this.``Simple seq of struct.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<TestStruct>) ->
                    buf.[0] <- buf.[1]
                    buf.[1] <- buf.[0]
            @>

        checkCode command "Simple.Seq.Struct.gen" "Simple.Seq.Struct.cl"

    [<Test>]
    member this.``Seq with bindings.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->
                    let x = 2
                    buf.[0] <- x
                    let y = 2
                    buf.[1] <- y
            @>

        checkCode command "Seq.With.Bindings.gen" "Seq.With.Bindings.cl"

    [<Test>]
    member this.``Bindings with equal names.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->
                    let x = 2
                    buf.[0] <- x
                    let x = 3
                    buf.[1] <- x
            @>

        checkCode command "Bindings.With.Equal.Names.gen" "Bindings.With.Equal.Names.cl"

    [<Test>]
    member this.``Binding and FOR counter conflict 1.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->
                    let i = 2
                    for i in 1..2 do     
                        buf.[1] <- i
            @>

        checkCode command "Binding.And.FOR.Counter.Conflict.1.gen" "Binding.And.FOR.Counter.Conflict.1.cl"

    [<Test>]
    member this.``Binding and FOR counter conflict 2.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->
                    for i in 1..2 do
                        let i = 2     
                        buf.[1] <- i
            @>

        checkCode command "Binding.And.FOR.Counter.Conflict.2.gen" "Binding.And.FOR.Counter.Conflict.2.cl"

    [<Test>]
    member this.``Binding and FOR counter conflict 3.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->
                    for i in 0..1 do
                        let i = i + 2
                        buf.[i] <- 2
            @>

        checkCode command "Binding.And.FOR.Counter.Conflict.3.gen" "Binding.And.FOR.Counter.Conflict.3.cl"

    [<Test>]
    member this.``Binding and FOR counter conflict 4.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->
                    let i = 1
                    for i in 0..i+1 do
                        let i = i + 2
                        buf.[i] <- 2
            @>

        checkCode command "Binding.And.FOR.Counter.Conflict.4.gen" "Binding.And.FOR.Counter.Conflict.4.cl"

    [<Test>]
    member this.``Quotations injections 1``() = 
        let myF = <@ fun x -> x * x @>
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->
                    buf.[0] <- (%myF) 2
                    buf.[1] <- (%myF) 4
            @>

        checkCode command "Quotations.Injections.1.gen" "Quotations.Injections.1.cl"

    [<Test>]
    member this.``Quotations injections 2``() = 
        let myF = <@ fun x y -> x - y @>
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->
                    buf.[0] <- (%myF) 2 3
                    buf.[1] <- (%myF) 4 5
            @>

        checkCode command "Quotations.Injections.2.gen" "Quotations.Injections.2.cl"

    //[<Test>]
    member this.``Nested functions``() =
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->
                    let f x y = x - y
                    buf.[0] <- f 2 3
                    buf.[1] <- f 4 5
            @>

        checkCode command "Nested.Function.gen" "Nested.Function.cl"

    //[<Test>]
    member this.``Nested functions. Carring.``() =
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->
                    let f x y = x - y
                    let g = f 2
                    buf.[0] <- g 3
                    buf.[1] <- g 5
            @>

        checkCode command "Nested.Function.Carring.gen" "Nested.Function.Carring.cl"

    //[<Test>]
    member this.``Nested functions. Carring22.``() =
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->                    
                    let f x y = 
                        let gg = ref 0
                        for i in 1..x do gg := !gg + y
                        !gg
                    let g x = f 2 x
                    buf.[0] <- g 2
                    buf.[1] <- g 3
            @>

        checkCode command "Nested.Function.Carring.gen" "Nested.Function.Carring.cl"

    member this.``Nested functions. Carring23.``() =
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int*int>) ->                                        
                    buf.[0] <- 2,3
                    buf.[1] <- 2,4
            @>

        checkCode command "Nested.Function.Carring.gen" "Nested.Function.Carring.cl"

    [<Test>]
    member this.``Let renamed``() =
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->                                        
                    let f x = 
                        let g = 1 + x
                        g
                    buf.[0] <- f 1
            @>

        checkCode command "Let renamed.gen" "Let renamed.cl"

    [<Test>]
    member this.``Let renamed 2``() =
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->                                        
                    let f m k = 
                        let g q w = 1 + q + w
                        let t p = 7 - p
                        (g 1 2) - m * k / (t 53)
                    buf.[0] <- f 1 4
            @>

        checkCode command "Let renamed 2.gen" "Let renamed 2.cl"

    [<Test>]
    member this.``Renamer Test``() =
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->                                        
                    let f x y = 
                        let y = y
                        let y = y
                        let g x m = m + x
                        g x y
                    buf.[0] <- f 1 7
            @>

        checkCode command "Renamer Test.gen" "Renamer Test.cl"

    [<Test>]
     member this.``Template Let Transformation Test 0``() =
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->                                        
                    let f = 3
                    buf.[0] <- f
            @>

        checkCode command "Template Test 0.gen" "Template Test 0.cl"

    [<Test>]
    member this.``Template Let Transformation Test 1``() =
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->                                        
                    let f = 
                        let x = 3
                        x
                    buf.[0] <- f
            @>

        checkCode command "Template Test 1.gen" "Template Test 1.cl"

    [<Test>]        
    member this.``Template Let Transformation Test 2``() =
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->                                        
                    let f = 
                        let x = 
                            let y = 3
                            y
                        x
                    buf.[0] <- f
            @>

        checkCode command "Template Test 2.gen" "Template Test 2.cl"

    [<Test>]
    member this.``Template Let Transformation Test 3``() =
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->                                        
                    let f = 
                        let f = 5
                        f
                    buf.[0] <- f
            @>

        checkCode command "Template Test 3.gen" "Template Test 3.cl"

    [<Test>]
    member this.``Template Let Transformation Test 4``() =
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->                                        
                    let f = 
                        let f = 
                            let f = 5
                            f
                        f
                    buf.[0] <- f
            @>

        checkCode command "Template Test 4.gen" "Template Test 4.cl"

    [<Test>]
    member this.``Template Let Transformation Test 5``() =
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->                                        
                    let f a b = 
                        let x y z = y + z
                        x a b
                    buf.[0] <- f 1 7
            @>

        checkCode command "Template Test 5.gen" "Template Test 5.cl"

    [<Test>]
    member this.``Template Let Transformation Test 6``() =
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->                                        
                    let f x y = 
                        let x = x
                        x + y
                    buf.[0] <- f 7 8
            @>
        checkCode command "Template Test 6.gen" "Template Test 6.cl"

    [<Test>]
    member this.``Template Let Transformation Test 7``() =
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->                                        
                    let f y = 
                        let x y = 6 - y
                        x y
                    buf.[0] <- f 7
            @>
        checkCode command "Template Test 7.gen" "Template Test 7.cl"

    [<Test>]    
    member this.``Template Let Transformation Test 8``() =
        let command = 
            <@ fun (range:_1D) (m:array<int>) -> 
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

    [<Test>]
    member this.``Template Let Transformation Test 9``() =
        let command = 
            <@ fun (range:_1D) (buf:array<int>) -> 
                    let x n = 
                        let r = 8
                        let h = r + n
                        h
                    buf.[0] <- x 9

            @>
        checkCode command "Template Test 9.gen" "Template Test 9.cl"

    [<Test>]
    member this.``Template Let Transformation Test 10``() =
        let command = 
            <@ fun (range:_1D) (buf:array<int>) -> 
                    let p = 9
                    let x n b = 
                        let t = 0
                        n + b + t
                    buf.[0] <- x 7 9
            @>
        checkCode command "Template Test 10.gen" "Template Test 10.cl"

    [<Test>]
    member this.``Template Let Transformation Test 11``() =
        let command = 
            <@ fun (range:_1D) (buf:array<int>) -> 
                    let p = 1
                    let m = 
                        let r l =
                            l + p
                        r 9
                    let z k = k + 1
                    buf.[0] <- m
            @>
        checkCode command "Template Test 11.gen" "Template Test 11.cl"

    [<Test>]
    member this.``Template Let Transformation Test 12``() =
        let command = 
            <@ fun (range:_1D) (buf:array<int>) -> 
                    let f x y =
                        let y = y
                        let y = y
                        let g x m = m + x
                        g x y
                    buf.[0] <- f 1 7
            @>
        checkCode command "Template Test 12.gen" "Template Test 12.cl"

    [<Test>]
    member this.``Template Let Transformation Test 13``() =
        let command = 
            <@ fun (range:_1D) (buf:array<int>) -> 
                    let f y =
                        let y = y
                        let y = y
                        let g m = m + 1
                        g y
                    buf.[0] <- f 7
            @>
        checkCode command "Template Test 13.gen" "Template Test 13.cl"

    [<Test>]
    member this.``Template Let Transformation Test 14``() =
        let command = 
            <@ fun (range:_1D) (buf:array<int>) -> 
                    let f (y:int) =
                        let y = y
                        let y = y
                        let g (m:int) = 
                            let g r t = r + y - t
                            let n o = o - (g y 2)
                            n 5
                        g y
                    let z y = y - 2
                    buf.[0] <- f (z 7)
            @>
        checkCode command "Template Test 14.gen" "Template Test 14.cl"

    [<Test>]
    member this.``Template Let Transformation Test 15``() =
        let command = 
            <@ fun (range:_1D) (buf:array<int>) -> 
                    let f y =
                        let Argi index =  
                            if index = 0 then buf.[1]
                            else buf.[2]
                        Argi y
                    buf.[0] <- f 0
            @>
        checkCode command "Template Test 15.gen" "Template Test 15.cl"

    [<Test>]
    member this.``Template Let Transformation Test 16``() =
        let command = 
            <@ fun (range:_1D) (buf:array<int>) -> 
                    let f y =
                        if y = 0
                        then 
                            let z a = a + 1
                            z 9
                        else buf.[2]
                    buf.[0] <- f 0
            @>
        checkCode command "Template Test 16.gen" "Template Test 16.cl"



//[<EntryPoint>]
//let f _ =
//    //(new Translator()).``Nested functions``()
//    //(new Translator()).``Simple seq of struct.``()
//    //(new Brahma.FSharp.OpenCL.Full.Translator()).``Simple seq of struct.``()
//    //(new Brahma.FSharp.OpenCL.Full.Translator()).
//        //``Binding in WHILE.``()
//        //``Template Let Transformation Test 18``()
//        //EigenCFA()
//    PerformanceCYK.answer()
//    0