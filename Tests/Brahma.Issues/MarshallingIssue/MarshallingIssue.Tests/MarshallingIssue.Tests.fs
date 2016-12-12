module MarshalingIssue.Tests

open NUnit.Framework
open System.IO
open System
open System.Reflection

open MarshallingIssue
open Brahma.Helpers
open OpenCL.Net
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Extensions
open NUnit.Framework

[<TestFixture>]
type ``Marshalling Issue`` () = 
    let GenerateMatrix64 rows cols =
        let random = new System.Random()
        Array.init (rows * cols) (fun i -> float (random.NextDouble()))

    let GenerateMatrix32 rows cols =
        let random = new System.Random()
        Array.init (rows * cols) (fun i -> float32 (random.NextDouble()))

    [<Test>]
    member this.``CheckFloat`` ()=
        let res = Float64Version "NVIDIA*" (GenerateMatrix64 200 200) (GenerateMatrix64 200 200)
        Assert.AreEqual (true, res)

    [<Test>]
    member this.``CheckFloat32`` ()=
        let res = Float32Version "NVIDIA*" (GenerateMatrix32 200 200) (GenerateMatrix32 200 200)
        Assert.AreEqual (true, res)