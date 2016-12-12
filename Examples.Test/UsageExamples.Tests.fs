module MarshalingIssue.Tests

open NUnit.Framework
open System.IO
open System
open System.Reflection

open MatrixExamples
open ArrayExamples
open FloatingPointExamples
open AgentsExamples
open Brahma.Helpers
open OpenCL.Net
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Extensions
open NUnit.Framework

[<TestFixture>]
type ``Usage Examples`` () = 

// Test values generators
    let random = new System.Random()   
     
    let GenerateMatrixInt64 rows cols range =
            Array.init (rows * cols) (fun i -> int64 (random.NextDouble() * range))

    let GenerateMatrixFP rows cols range =
        Array.init (rows * cols) (fun i -> float (random.NextDouble() * range))

    let GenerateArrayFP length range =
        Array.init (length) (fun i -> float (random.NextDouble() * range))

    let GenerateArrayInt length range =
        Array.init (length) (fun i -> int (random.NextDouble() * range))
    
// Testing functions
    let Add (a:array<_>) (b:array<_>)  row column =
        let mutable c = Array.zeroCreate (row*column)
        for i in 0 .. row - 1 do
            for j in 0 .. column - 1 do
                c.[i * column + j] <- a.[i * column + j] + b.[i*column + j]
        c

    let Multiply (a:array<int64>) b row column =
        let mutable c = Array.zeroCreate (row*column)
        for i in 0 .. row - 1 do
            for j in 0 .. column - 1 do
                c.[i*column + j] <- a.[i*column + j] * b
        c

    let TriangularMatrix (a:array<float>) n =     
        for i in 0 .. n - 1 do
            for j in i + 1 .. n - 1 do
                let f = a.[j * n + i] / a.[i * n + i]     
                for k in 0 .. n - 1 do
                    a.[j * n + k] <- a.[j * n + k] - a.[i * n + k] * f
        a

    let Fibonacci length =
        let a = Array.init length (fun i -> int64(1))
        for i in 2 .. length - 1 do
            a.[i] <- a.[i - 1] + a.[i - 2]
        a

    let Reflection (a:array<_>) length =
        for i in 0 .. length / 2 do
            let buff = a.[i]
            a.[i] <- a.[length - i - 1]
            a.[length - 1 - i] <- a.[i]
        a

    let OddEvenArray length = 
        let arr = Array.init length (fun i -> i)
        
        let rec pow (a, b) =
            match b with 
                | 0 -> 1
                | 1 -> a
                | _ -> match b % 2 with
                           | 1 -> pow(a, b / 2) * pow(a, b / 2) * a
                           | 0 -> pow(a, b / 2) * pow(a, b / 2)
                           | _ -> 1

        
        for i in 0 .. length - 1 do
            if i % 2 = 1 
            then arr.[i] <- pow(arr.[i], 4) + pow(arr.[i], 2) + 1
            else arr.[i] <- pow(arr.[i], 5) + pow(arr.[i], 3) + arr.[i]
        arr

    [<Test>]
    member this.``Addition`` ()=
        let r = 200
        let c = 900
        let b = 10.0
        let m1 = GenerateMatrixFP r c b
        let m2 = GenerateMatrixFP r c b
        let control = Add m1 m2 r c
        let res = Addition m1 m2 r c
        Assert.AreEqual (res, control)

    [<Test>]
    member this.``Constant And Matrix Multiply``()=
        let rows = 1000
        let columns = 300
        let range = 50.0
        let constant = int64(3)
        let m1 = GenerateMatrixInt64 rows columns range
        let control = Multiply m1 constant rows columns
        let res = ConstantAndMatrixMultiply m1 constant rows columns
        Assert.AreEqual (control, res)

    [<Test>]
    member this.``Triangular Matrix``()=
        let r = 30
        let b = 10.0
        let m1 = GenerateMatrixFP r r b
        let control = TriangularMatrix m1 r
        let res = TriMat m1 r r
        for i in 0 .. r - 1 do
            for j in 0 .. r - 1 do
                control.[i * r + j] <- Math.Round(control.[i * r + j], 10)
                res.[i * r + j] <- Math.Round(control.[i * r + j], 10)
        Assert.AreEqual (control, res)
    
    [<Test>]
    member this.``Fibonacci row`` ()=
        let l = 80
        let res = Fib l
        let control = Fibonacci l
        Assert.AreEqual (control, res)

    [<Test>]
    member this.``Array Nulification`` ()=
        let l = 80
        let r = 50.0
        let e1 = 59
        let e2 = 78
        let arr = GenerateArrayInt l r
        let control = arr
        for i in e1 .. e2 do 
                control.[i] <- 0
        let res = Nulify arr l e1 e2
        Assert.AreEqual (control, res)

    [<Test>]
    member this.``Array Reflection`` ()=
        let l = 80
        let r = 50.0
        let arr = GenerateArrayInt l r
        let control = Reflection arr l
        let res = Reflect arr l
        Assert.AreEqual (control, res)

    [<Test>]
    member this.``Cartesian to spherical coordinates convertion`` ()=
        let a = [|2.5; 4.6; 7.8|]
        let r = sqrt(2.5 * 2.5 + 4.6 * 4.6 + 7.8 * 7.8)
        let control = [|r; acos(7.8 / r); atan(4.6 / 2.5)|] 
        let res = CartesianToSpherical a
        Assert.AreEqual (control, res)

    [<Test>]
    member this.``Pyramid volume`` ()=
        let a = [|2.0; 2.0; 2.0; 2.0; 2.0; 2.0; 0.0|]
        let control = [|2.0; 2.0; 2.0; 2.0; 2.0; 2.0; 2.0 * sqrt(2.0) / 3.0|]
        let res = PyramidVolume a
        Assert.AreEqual (Math.Round(control.[6], 10), Math.Round(res.[6], 10))

    [<Test>]
    member this.``LaGrange Interpolation`` ()=
        let p = 3.14159
        let a = [|0.0; p / 6.0; p / 4.0; p / 3.0; p / 2.0; p / 3.6|]
        let b = Array.zeroCreate 6 
        for i in 0 .. 4 do
            b.[i] <- sin(a.[i])
        let res = LaGrangeInterpol a b 6
        Assert.AreEqual (Math.Round(sin(a.[5]), 4), Math.Round(res.[5], 4))

    [<Test>]
    member this.``Worker Agent Example`` ()=
        let length = 50
        let res = working length
        let control = OddEvenArray length
        Assert.AreEqual (control, res)