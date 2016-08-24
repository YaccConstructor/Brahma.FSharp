module Brahma.FSharp.OpenCL.Full

open NUnit.Framework
open System.IO
open System
open System.Reflection


open Brahma.Helpers
open OpenCL.Net
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open System
open System.Reflection
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Extensions

[<Struct>]
type TestStruct =
    val mutable x: int 
    val mutable y: float
    new (x,y) = {x=x; y=y} 

[<TestFixture>]
type Translator() =
    let defaultInArrayLength = 4
    let intInArr = [|0..defaultInArrayLength-1|]
    let float32Arr = Array.init defaultInArrayLength (fun i -> float32 i)
    let _1d = new _1D(defaultInArrayLength, 1)
    let _2d = new _2D(defaultInArrayLength, 1)
    let deviceType = DeviceType.Default
    let platformName = "NVIDIA*"

    let provider =
        try  ComputeProvider.Create(platformName, deviceType)
        with
        | ex -> failwith ex.Message
 
    let checkResult command =
        let kernel,kernelPrepareF, kernelRunF = provider.Compile command    
        let commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)            
        let check (outArray:array<'a>) (expected:array<'a>) =        
            let cq = commandQueue.Add(kernelRunF()).Finish()
            let r = Array.zeroCreate expected.Length
            let cq2 = commandQueue.Add(outArray.ToHost(provider,r)).Finish()
            commandQueue.Dispose()
            Assert.AreEqual(expected, r)
            provider.CloseAllBuffers()
        kernelPrepareF,check

    let returnResult command =
        let kernel, kernelPrepareF, kernelRunF = provider.Compile command    
        let commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)            
        let getresult (outArray: array<_>) =
            let cq = commandQueue.Add(kernelRunF()).Finish()
            let cq2 = commandQueue.Add(outArray.ToHost provider).Finish()
            commandQueue.Dispose()
            provider.CloseAllBuffers()
            outArray
        kernelPrepareF, getresult
          
    let matrixToArray (m: array<array<_>>) = 
        let lines = m.Length
        let cols = m.[0].Length 
        let arr = Array.zeroCreate (lines * cols)
        let s = seq {for i in 0..lines - 1 -> m.[i]}
        let arr = Array.concat s
        arr 
    
    let arrayToMatrix (arr: array<_>) lines cols = 
        let matrix = Array.zeroCreate (lines)
        for i in 0..lines - 1 do
            matrix.[i] <- (Array.sub arr (i * cols) cols)
        matrix
    
    let sumMatr (m1: array<array<_>>) (m2: array<array<_>>) = 
        let lines = m1.Length
        let cols = m1.[0].Length 
        let command = 
            <@ 
                 fun (rng: _1D) (m1: array<_>) (m2: array<_>) (res: array<_>) ->                    
                    let r = rng.GlobalID0
                    res.[r] <- m1.[r] + m2.[r]
            @>
        let res = Array.zeroCreate (lines * cols)
        let rng = new _1D(lines * cols, 1)
        let run, getresult = returnResult command
        run rng (matrixToArray m1) (matrixToArray m2) res
        arrayToMatrix (getresult res) lines cols

    let sumEl (m: array<array<_>>) = 
        let lines = m.Length
        let cols = m.[0].Length 
        let command = 
            <@ fun (rng:_1D) (m: array<_>) (res: array<_>) ->                    
                    let r = rng.GlobalID0
                    res.[0] <!+ m.[r]
            @>
        let res = Array.zeroCreate 1
        let rng = new _1D(lines * cols, 1)
        let run, getresult = returnResult command
        run rng (matrixToArray m) res
        getresult res

    [<Test>]
    member this.``Array item set``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) -> 
                    buf.[0] <- 1
            @>

        let run,check = checkResult command
        run _1d intInArr        
        check intInArr [|1;1;2;3|]

    [<Test>]
    member this.``Array item set. Long``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int64>) -> 
                    buf.[0] <- 1L
            @>

        let run,check = checkResult command
        let initInArr = [|0L; 1L; 2L; 3L|]
        run _1d initInArr
        check initInArr [|1L; 1L; 2L; 3L|]

    [<Test>]
    member this.``Cast. Long``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int64>) -> 
                    buf.[0] <- (int64)1UL
            @>

        let run,check = checkResult command
        let initInArr = [|0L; 1L|]
        run _1d initInArr
        check initInArr [|1L; 1L|]

    [<Test>]
    member this.``Cast. ULong``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<uint64>) -> 
                    buf.[0] <- (uint64)1L
            @>

        let run,check = checkResult command
        let initInArr = [|0UL; 1UL|]
        run _1d initInArr
        check initInArr [|1UL; 1UL|]

    [<Test>]
    member this.``Array item set. ULong``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<uint64>) -> 
                    buf.[0] <- 1UL
            @>

        let run,check = checkResult command
        let initInArr = [|0UL; 1UL; 2UL; 3UL|]
        run _1d initInArr
        check initInArr [|1UL; 1UL; 2UL; 3UL|]                

    [<Test>]
    member this.Binding() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) -> 
                    let x = 1
                    buf.[0] <- x
            @>
        
        let run,check = checkResult command
        run _1d intInArr        
        check intInArr [|1;1;2;3|]

    [<Test>]
    member this.``Binop plus``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) -> 
                    buf.[0] <- 1 + 2
            @>
        
        let run,check = checkResult command
        run _1d intInArr
        check intInArr [|3;1;2;3|]

    [<Test>]
    member this.``If Then``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) -> 
                    if 0 = 2 then buf.[0] <- 1
            @>
        
        let run,check = checkResult command
        run _1d intInArr
        check intInArr [|0;1;2;3|]

    [<Test>]
    member this.``If Then Else``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) -> 
                    if 0 = 2 then buf.[0] <- 1 else buf.[0] <- 2
            @>
        
        let run,check = checkResult command
        run _1d intInArr
        check intInArr [|2;1;2;3|]

    [<Test>]
    member this.``For Integer Loop``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) -> 
                    for i in 1..3 do buf.[i] <- 0
            @>
        
        let run,check = checkResult command
        run _1d intInArr
        check intInArr [|0;0;0;0|]

    [<Test>]
    member this.``Sequential bindings``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) -> 
                    let x = 1
                    let y = x + 1
                    buf.[0] <- y
            @>
        
        let run,check = checkResult command
        run _1d intInArr
        check intInArr [|2;1;2;3|]

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
        
        let run,check = checkResult command
        run _1d intInArr
        check intInArr [|2;1;2;3|]

    [<Test>]
    member this.``Binding in FOR.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) -> 
                    for i in 0..3 do
                        let x = i * i
                        buf.[0] <- x
            @>
        
        let run,check = checkResult command
        run _1d intInArr
        check intInArr [|9;1;2;3|] 

    [<Test>]
    member this.``WHILE loop simple test.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->
                 while buf.[0] < 5 do
                     buf.[0] <- buf.[0] + 1
            @>
        
        let run,check = checkResult command
        run _1d intInArr
        check intInArr [|5;1;2;3|]

    [<Test>]
    member this.``WHILE in FOR.``() = 
        let command = 
            <@
                fun (range:_1D) (buf:array<int>) ->
                 for i in 0..3 do
                     while buf.[i] < 10 do
                         buf.[i] <- buf.[i] * buf.[i] + 1
            @>
        
        let run,check = checkResult command
        run _1d intInArr
        check intInArr [|26;26;26;10|]

    [<Test>]
    member this.``Binding in WHILE.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->
                    while buf.[0] < 5 do
                        let x = buf.[0] + 1
                        buf.[0] <- x * x
            @>
        
        let run,check = checkResult command
        run _1d intInArr
        check intInArr [|25;1;2;3|]

    [<Test>]
    member this.``Simple 1D.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->
                    let i = range.GlobalID0
                    buf.[i] <- i + i
            @>
        
        let run,check = checkResult command
        run _1d intInArr
        check intInArr [|0;2;4;6|]

    [<Test>]
    member this.``Simple 1D with copy.``() = 
        let command = 
            <@ 
                fun (range:_1D) (inBuf:array<int>) (outBuf:array<int>) ->
                    let i = range.GlobalID0
                    outBuf.[i] <- inBuf.[i]
            @>
        
        let run,check = checkResult command
        let outA = [|0;0;0;0|]
        run _1d intInArr outA        
        check outA [|0;1;2;3|]

    [<Test>]
    member this.``Simple 1D float.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<float32>) ->
                    let i = range.GlobalID0
                    buf.[i] <- buf.[i] * buf.[i]
            @>
        
        let run,check = checkResult command
        run _1d float32Arr        
        check float32Arr [|0.0f;1.0f;4.0f;9.0f|]

    [<Ignore>]
    [<Test>]
    member this.``Math sin``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<float>) ->
                    let i = range.GlobalID0
                    buf.[i] <- System.Math.Sin (float buf.[i])
            @>
        
        let run,check = checkResult command
        let inA = [|0.0;1.0;2.0;3.0|]
        run _1d inA
        check inA (inA |> Array.map System.Math.Sin)  //[|0.0; 0.841471; 0.9092974; 0.14112|]        

    [<Test>]
    member this.``Int as arg``() = 
        let command = 
            <@ 
                fun (range:_1D) x (buf:array<int>) ->
                    let i = range.GlobalID0
                    buf.[i] <- x + x
            @>
        let run,check = checkResult command
        run _1d 2 intInArr        
        check intInArr [|4;4;4;4|]

    [<Test>]
    member this.``Sequential commands over single buffer``() = 
        let command = 
            <@ 
                fun (range:_1D) i x (buf:array<int>) ->
                    buf.[i] <- x + x
            @>        
        let kernel,kernelPrepareF, kernelRunF = provider.Compile command
        kernelPrepareF _1d 0 2 intInArr
        let commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)        
        let _ = commandQueue.Add(kernelRunF())
        kernelPrepareF _1d 2 2 intInArr
        let _ = commandQueue.Add(kernelRunF())
        let _ = commandQueue.Finish()
        let r = Array.zeroCreate intInArr.Length
        let _ = commandQueue.Add(intInArr.ToHost(provider, r)).Finish()
        commandQueue.Dispose()
        let expected = [|4;1;4;3|] 
        Assert.AreEqual(expected, r)
        provider.CloseAllBuffers()

    [<Test>]
    member this.``Sequential operations``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->
                    buf.[0] <- 2
                    buf.[1] <- 4
            @>
        let run,check = checkResult command
        run _1d intInArr
        check intInArr [|2;4;2;3|]

    [<Test>]
    member this.``Quotations injections 1``() = 
        let myF = <@ fun x -> x * x @>
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->
                    buf.[0] <- (%myF) 2
                    buf.[1] <- (%myF) 4
            @>
        let run,check = checkResult command
        run _1d intInArr
        check intInArr [|4;16;2;3|]

    [<Test>]
    member this.``Quotations injections 2``() = 
        let myF = <@ fun x y -> y - x @>
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->
                    buf.[0] <- (%myF) 2 5
                    buf.[1] <- (%myF) 4 9
            @>
        let run,check = checkResult command
        run _1d intInArr
        check intInArr [|3;5;2;3|]

    //[<Test>]
    member this.``Forward pipe``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->
                    buf.[0] <- 1.25f |> int
            @>
        let run,check = checkResult command
        run _1d intInArr
        check intInArr [|1;1;2;3|]

    //[<Test>]
    member this.``Backward pipe``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->
                    buf.[0] <- int <| 1.25f + 2.34f
            @>
        let run,check = checkResult command
        run _1d intInArr
        check intInArr [|3;1;2;3|]

    [<Test>]
    member this.``Byte type support``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<byte>) ->
                    buf.[0] <- buf.[0] + 1uy
                    buf.[1] <- buf.[1] + 1uy
                    buf.[2] <- buf.[2] + 1uy
            @>
        let run,check = checkResult command
        let inByteArray = [|0uy;255uy;254uy|]
        run _1d inByteArray
        check inByteArray [|1uy;0uy;255uy|]

    [<Test>]
    member this.``Write buffer``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<byte>) ->
                    buf.[0] <- buf.[0] + 1uy
                    buf.[1] <- buf.[1] + 1uy
                    buf.[2] <- buf.[2] + 1uy
            @>
        let kernel,kernelPrepareF, kernelRunF = provider.Compile command
        let inArray = [|1uy;2uy;3uy|]
        kernelPrepareF _1d inArray
        let commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)        
        let _ = commandQueue.Add(kernelRunF())
        let _ = commandQueue.Add(inArray.ToHost provider).Finish()
        let expected = [|2uy;3uy;4uy|] 
        Assert.AreEqual(expected, inArray)
        inArray.[0] <- 5uy
        commandQueue.Add(inArray.ToGpu provider) |> ignore
        let _ = commandQueue.Add(kernelRunF())
        let _ = commandQueue.Add(inArray.ToHost provider).Finish()
        let expected = [|6uy;4uy;5uy|] 
        Assert.AreEqual(expected, inArray)
        commandQueue.Dispose()        
        provider.CloseAllBuffers()

    [<Test>]
    member this.``Buffers initialisation``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<byte>) ->
                    buf.[0] <- buf.[0] + 1uy
                    buf.[1] <- buf.[1] + 1uy
                    buf.[2] <- buf.[2] + 1uy
            @>
        let kernel,kernelPrepareF, kernelRunF = provider.Compile command
        let inArray = [|1uy;2uy;3uy|]
        kernelPrepareF _1d inArray
        let commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)        
        let _ = commandQueue.Add(inArray.ToGpu(provider,[|2uy;3uy;4uy|]))
        let _ = commandQueue.Add(kernelRunF())
        let _ = commandQueue.Add(inArray.ToHost provider).Finish()
        let expected = [|3uy;4uy;5uy|] 
        Assert.AreEqual(expected, inArray)
        commandQueue.Dispose()        
        provider.CloseAllBuffers()

    [<Test>]
    member this.``While with preheader.``() = 
        let command = 
            <@
                fun (rng:_1D) ->
                    let mutable parent = 10s

                    while parent > 0s do
                        let currentTemplate = 10s
                        parent <- parent - 1s
                
            @>
        
        let kernel,kernelPrepare, kernelRun = provider.Compile command                
        let commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)
        kernelPrepare _1d
        try 
            commandQueue.Add(kernelRun()).Finish()
            |> ignore
            commandQueue.Dispose()
        with e -> 
            commandQueue.Dispose()
            Assert.Fail e.Message
         
    [<Test>]
    member this.``Double on GPU.``() =
        let command = 
            <@                            
                fun (r:_2D) ->                
                    let x = r.GlobalID0
                    let y = r.GlobalID1
                    let scaling = 0.5 
                    let b = ref 0.0
                    let size : float = 100.0
                    let fx = float x / size * scaling + float -1.5
                    b := 0.0
            @>

        let kernel,kernelPrepare, kernelRun = provider.Compile command                
        let commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)       
        kernelPrepare _2d
        try 
            commandQueue.Add(kernelRun()).Finish()
            |> ignore
            commandQueue.Dispose()
        with e -> 
            commandQueue.Dispose()
            Assert.Fail e.Message

    [<Test>]
    member this.``Simple seq of struct.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<TestStruct>) ->
                    let b = buf.[0]
                    buf.[0] <- buf.[1]
                    buf.[1] <- b
            @>
        let run,check = checkResult command
        let inByteArray = [|new TestStruct(1, 2.0);new TestStruct(3, 4.0)|]
        run _1d inByteArray
        check inByteArray [|new TestStruct(3, 4.0); new TestStruct(1, 2.0)|]

    //[<Test>]
    member this.``Simple seq of struct changes.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<TestStruct>) ->
                    buf.[0] <- new TestStruct(5,6.0)                    
            @>
        let run,check = checkResult command
        let inByteArray = [|new TestStruct(1, 2.0);new TestStruct(3, 4.0)|]
        run _1d inByteArray
        check inByteArray [|new TestStruct(3, 4.0); new TestStruct(1, 2.0)|]

    [<Test>]
    member this.``Simple seq of struct prop set.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<TestStruct>) ->
                    buf.[0].x <- 5
            @>
        let run,check = checkResult command
        let inByteArray = [|new TestStruct(1, 2.0)|]
        run _1d inByteArray
        check inByteArray [|new TestStruct(5, 2.0)|]

    [<Test>]
    member this.``Simple seq of struct prop get.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<TestStruct>) ->
                    buf.[0].x <- buf.[1].x + 1                    
            @>
        let run,check = checkResult command
        let inByteArray = [|new TestStruct(1, 2.0);new TestStruct(3, 4.0)|]
        run _1d inByteArray
        check inByteArray [|new TestStruct(4, 2.0); new TestStruct(3, 4.0)|]

    [<Test>]
    member this.``Atomic max.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<_>) (b:array<_>) ->
                    b.[0] <- aMaxR buf.[0] 2
            @>
        let run,check = checkResult command
        let inByteArray = [|1|]
        run _1d inByteArray [|0|]
        check inByteArray [|2|]

    [<Test>]
    member this.``Atomic max 2.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<_>) (b:array<_>) ->
                    b.[0] <- aMaxR buf.[0] 1
            @>
        let run,check = checkResult command
        let inByteArray = [|2|]
        run _1d inByteArray [|0|]
        check inByteArray [|2|]

    [<Test>]
    member this.``Atomic min.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<_>) (b:array<_>) ->
                    b.[0] <- aMinR buf.[0] 2
            @>
        let run,check = checkResult command
        let inByteArray = [|1|]
        run _1d inByteArray [|0|]
        check inByteArray [|1|]

    [<Test>]
    member this.``Atomic min 2.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<_>) (b:array<_>) ->
                    b.[0] <- aMinR buf.[0] 1
            @>
        let run,check = checkResult command
        let inByteArray = [|2|]
        run _1d inByteArray [|0|]
        check inByteArray [|1|]

    [<Test>]
    member this.``Atomic exchange.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<_>) (b:array<_>) ->
                    b.[0] <- buf.[0] <!>  2
            @>
        let run,check = checkResult command
        let inByteArray = [|1|]
        run _1d inByteArray [|0|]
        check inByteArray [|2|]

    [<Test>]
    member this.``Atomic exchange 2.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<_>) ->
                    buf.[0] <! 2
            @>
        let run,check = checkResult command
        let inByteArray = [|1|]
        run _1d inByteArray
        check inByteArray [|2|]

    [<Test>]
    member this.``Atomic decr return.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<_>) ->
                    buf.[1] <- aDecrR buf.[0]
            @>
        let run,check = checkResult command
        let inByteArray = [|0;0;0;0|]
        run _1d inByteArray
        check inByteArray [|-4;-3;0;0|]

    [<Test>]
    member this.``Atomic decr.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<_>) ->
                    aDecr buf.[0]
            @>
        let run,check = checkResult command
        let inByteArray = [|0;0;0;0|]
        run _1d inByteArray
        check inByteArray [|-4;0;0;0|]

    [<Test>]
    member this.``Atomic incr return.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<_>) ->
                    buf.[1] <- aIncrR buf.[0]
            @>
        let run,check = checkResult command
        let inByteArray = [|1;2;0;0|]
        run _1d inByteArray
        check inByteArray [|5;4;0;0|]

    [<Test>]
    member this.``Atomic incr.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<_>) ->
                    aIncr buf.[0]
            @>
        let run,check = checkResult command
        let inByteArray = [|0;0;0;0|]
        run _1d inByteArray
        check inByteArray [|4;0;0;0|]

    [<Test>]
    member this.``Atomic compare exchange return.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<_>) ->
                    buf.[1] <- aCompExchR buf.[0] 1 2
            @>
        let run,check = checkResult command
        let inByteArray = [|1;0;0;0|]
        run _1d inByteArray
        check inByteArray [|2;2;0;0|]

    [<Test>]
    member this.``Atomic compare exchange.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<_>) ->
                    aCompExch buf.[0] 1 2
            @>
        let run,check = checkResult command
        let inByteArray = [|1;0;0;0|]
        run _1d inByteArray
        check inByteArray [|2;0;0;0|]

    [<Test>]
    member this.``Atomic compare exchange 2.``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<_>) ->
                    aCompExch buf.[0] 1 2
            @>
        let run,check = checkResult command
        let inByteArray = [|3;0;0;0|]
        run _1d inByteArray
        check inByteArray [|3;0;0;0|]


    [<Test>]
    member this.``Template Let Transformation Test 9``() = 
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) -> 
                        let x n = 
                            let mutable r = 8
                            let mutable h = r + n
                            h
                        buf.[0] <- x 9
            @>

        let run,check = checkResult command
        run _1d intInArr        
        check intInArr [|17;1;2;3|]        

    [<Test>]
    member this.``createStartStoreKernel``() = 
        let command = 
                <@ fun (r:_2D) (devStore:array<_>) (scaleExp) (scaleM:int) (scaleVar:int) -> 
                        let column = r.GlobalID0
                        let row = r.GlobalID1

                        if row < scaleExp && column < scaleM
                        then 
                            if row < scaleVar
                            then
                                if column % scaleM = 0
                                then devStore.[row*scaleM + column] <- 1
                                else devStore.[row*scaleM + column] <- -1
                            elif column = 0
                            then devStore.[row*scaleM + column] <- 2
                            elif column = 1
                            then devStore.[row*scaleM + column] <- row - scaleVar + 1
                            else devStore.[row*scaleM + column] <- -1 
                @>

        let initStore,check = checkResult command
        let intArr = Array.zeroCreate 45
        initStore (new _2D(5, 9)) intArr 9 5 6      
        check intArr   [|1;-1;-1;-1;-1;
                         1;-1;-1;-1;-1;
                         1;-1;-1;-1;-1;
                         1;-1;-1;-1;-1;  
                         1;-1;-1;-1;-1;
                         1;-1;-1;-1;-1;
                         2; 1;-1;-1;-1;
                         2; 2;-1;-1;-1;
                         2; 3;-1;-1;-1
        |]

    [<Test>]
    member this.twoFun() = 
        let command = 
                <@ fun (r:_1D) (devStore:array<int>) -> 
                        let x y = 
                            devStore.[0] <- devStore.[0] + 1
                            y + 2
                        devStore.[1] <- x 9
                @>

        let kernel,kernelPrepareF, kernelRunF = provider.Compile command    
        let commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)        
        let run,check = checkResult command
        run _1d intInArr
        check intInArr [|1; 11; 2; 3|]


    [<Test>]
    member this.EigenCFA() = 
        let command = 
                <@ fun (r:_2D) (devStore:array<_>) (scaleExp) (scaleM:int) (scaleVar:int) -> 
                        let column = r.GlobalID0
                        let row = r.GlobalID1

                        if row < scaleExp && column < scaleM
                        then 
                            if row < scaleVar
                            then
                                if column % scaleM = 0
                                then devStore.[row*scaleM + column] <- 1
                                else devStore.[row*scaleM + column] <- -1
                            elif column = 0
                            then devStore.[row*scaleM + column] <- 2
                            elif column = 1
                            then devStore.[row*scaleM + column] <- row - scaleVar + 1
                            else devStore.[row*scaleM + column] <- -1 
                @>

        let qEigenCFA = 
                <@ fun (r:_2D)
                    (devFun:array<_>)
                    (devArg1:array<_>)
                    (devArg2:array<_>)
                    (devStore:array<_>)
                    (devRep:array<_>)
                    devScaleM
                    devScaleCall
                    devScaleLam ->
                       let column = r.GlobalID0
                       let row = r.GlobalID1
                       if column < devScaleCall && row < 2
                       then
                            let numCall = column
                            let Argi index =  
                                if index = 0
                                then devArg1.[numCall]
                                else devArg2.[numCall]
                            let L index = devStore.[devFun.[numCall]*devScaleM + index]
                            let Li index = devStore.[(Argi row)*devScaleM + index]
                            let rowStore row column = devStore.[row*devScaleM + column]
                            let vL j =
                                if row = 0
                                then (L j) - 1
                                else (L j) - 1 + devScaleLam
                            for j in 1 .. ((L 0) - 1) do
                                for k in 1 .. ((Li 0) - 1) do
                                    let mutable isAdd = 1
                                    let addVar = Li k
                                    for i in 1 .. (rowStore (vL j) 0) - 1 do
                                        if rowStore (vL j) i = addVar
                                        then isAdd <- 0
                                    if isAdd > 0 then
                                        devRep.[0] <- devRep.[0] + 1
                                        let tail = (rowStore (vL j) 0)
                                        devStore.[(vL j)*devScaleM] <- devStore.[(vL j)*devScaleM] + 1
                                        devStore.[(vL j)*devScaleM + tail] <- addVar
                @>
//        let initStore,check = checkResult command
//        let intArr = Array.zeroCreate 45
//        initStore (new _2D(5, 9)) intArr 9 5 6   
        
        let intArr =  [|1;-1;-1;-1;-1;
                        1;-1;-1;-1;-1;
                        1;-1;-1;-1;-1;
                        1;-1;-1;-1;-1;  
                        1;-1;-1;-1;-1;
                        1;-1;-1;-1;-1;
                        2; 1;-1;-1;-1;
                        2; 2;-1;-1;-1;
                        2; 3;-1;-1;-1
                        |]

        let kernel,kernelPrepareF, kernelRunF = provider.Compile qEigenCFA    
        let commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head) 

//        let EigenCFAStart,checkCFA = checkResult qEigenCFA

        let mutable rep = -1
        let mutable curRep = -2
        let mutable iter = 1

        let Fun = [|5;1;0;8|]
        let Arg1 = [|5;4;3;7|]
        let Arg2 = [|5;4;3;6|]
        let repArray = Array.zeroCreate 1

        while(rep <> curRep) do
            iter <- iter + 1
            rep <- curRep
            let EigenCFA = 
                kernelPrepareF (new _2D(4, 2)) Fun Arg1 Arg2 intArr repArray 5 4 3
                let cq = commandQueue.Add(kernelRunF()).Finish()
                let r = Array.zeroCreate 1
                let cq2 = commandQueue.Add(repArray.ToHost(provider,r)).Finish()
                printf "%A\n" r
                r
            let a = EigenCFA
            curRep <- a.[0]

        
        let expectedResult =  [|2; 1;-1;-1;-1;
                                1;-1;-1;-1;-1;
                                2; 2;-1;-1;-1;
                                2; 1;-1;-1;-1;  
                                1;-1;-1;-1;-1;
                                2; 1;-1;-1;-1;
                                2; 1;-1;-1;-1;
                                2; 2;-1;-1;-1;
                                2; 3;-1;-1;-1
                                |]

        let cq = commandQueue.Add(kernelRunF()).Finish()
        let r = Array.zeroCreate 45
        let cq2 = commandQueue.Add(intArr.ToHost(provider,r)).Finish()

        Assert.AreEqual(expectedResult, r)

        provider.CloseAllBuffers()

    [<Test>]
    member this.``Template Let Transformation Test 0``() =
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->                                        
                    let f = 3
                    buf.[0] <- f
            @>

        let run,check = checkResult command
        run _1d intInArr        
        check intInArr [|3;1;2;3|]

    [<Test>]
    member this.``Template Let Transformation Test 1``() =
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) -> 
                    let x = 4                                       
                    let f = 
                        let x = 3
                        x
                    buf.[0] <- x + f
            @>
        let run,check = checkResult command
        run _1d intInArr
        check intInArr [|7;1;2;3|]

    [<Test>]
    member this.``Template Let Transformation Test 1.2``() =
        let command = 
            <@ 
                fun (range:_1D) (buf:array<int>) ->
                    let f y = 
                        let x c b = b + c + 4 + y
                        x 2 3
                    buf.[0] <- f 1
            @>
        let run,check = checkResult command
        run _1d intInArr        
        check intInArr [|10;1;2;3|]

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

        let run,check = checkResult command
        run _1d intInArr        
        check intInArr [|3;1;2;3|]

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

        let run,check = checkResult command
        run _1d intInArr        
        check intInArr [|5;1;2;3|]

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

        let run,check = checkResult command
        run _1d intInArr        
        check intInArr [|5;1;2;3|]

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

        let run,check = checkResult command
        run _1d intInArr        
        check intInArr [|8;1;2;3|]

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

        let run,check = checkResult command
        run _1d intInArr        
        check intInArr [|15;1;2;3|]        

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

        let run,check = checkResult command
        run _1d intInArr        
        check intInArr [|-1;1;2;3|]

    [<Test>]
    member this.``Template Let Transformation Test 8``() =
        let command = 
            <@ fun (range:_1D) (m:array<int>) -> 
                    let p = m.[0]
                    let x n = 
                        let l = m.[3]
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

        let run,check = checkResult command
        run _1d intInArr        
        check intInArr [|-1;1;2;3|]

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

        let run,check = checkResult command
        run _1d intInArr        
        check intInArr [|16;1;2;3|]


    [<Test>]
    member this.``Template Let Transformation Test 11``() =
        let command = 
            <@ fun (range:_1D) (buf:array<int>) -> 
                    let p = 1
                    let m = 
                        let r (l:int) = l
                        r 9
                    let z (k:int) = k
                    buf.[0] <- m
            @>

        let run,check = checkResult command
        run _1d intInArr        
        check intInArr [|9;1;2;3|]

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

        let run,check = checkResult command
        run _1d intInArr        
        check intInArr [|8;1;2;3|]

    [<Test>]
    member this.``Template Let Transformation Test 13``() =
        let command = 
            <@ fun (range:_1D) (buf:array<int>) -> 
                    let f y =
                        let y = y
                        let y = y
                        let g (m:int) = m
                        g y
                    buf.[0] <- f 7
            @>

        let run,check = checkResult command
        run _1d intInArr        
        check intInArr [|7;1;2;3|]

    [<Test>]
    member this.``Template Let Transformation Test 14``() =
        let command = 
            <@ fun (range:_1D) (buf:array<int>) -> 
                    let f y =
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

        let run,check = checkResult command
        run _1d intInArr        
        check intInArr [|-3;1;2;3|]

    [<Test>]
    member this.``Template Let Transformation Test 15``() =
        let command = 
            <@ fun (range:_1D) (buf:array<int>) -> 
                    let f y =
                        let Argi index =  
                            if index = 0
                            then buf.[1]
                            else buf.[2]
                        Argi y
                    buf.[0] <- f 0
            @>

        let run,check = checkResult command
        run _1d intInArr        
        check intInArr [|1;1;2;3|]

    [<Test>]
    member this.``Template Let Transformation Test 16``() =
        let command = 
            <@ fun (range:_1D) (buf:array<int>) -> 
                    let f y =
                        if y = 0
                        then 
                            let z (a:int) = a
                            z 9
                        else buf.[2]
                    buf.[0] <- f 0
            @>

        let run,check = checkResult command
        run _1d intInArr        
        check intInArr [|9;1;2;3|]

    [<Test>]
    member this.``Template Let Transformation Test 17``() =
        let command = 
            <@ fun (range:_1D) (buf:array<int>) -> 
                    let f y =
                        let g = buf.[1] + 1
                        y + g
                    for i in 0..3 do
                       buf.[i] <- f i
            @>

        let run,check = checkResult command
        run _1d intInArr        
        check intInArr [|2;3;6;7|]

    [<Test>]
    member this.``Template Let Transformation Test 18``() =
        let command = 
            <@ fun (range:_1D) (buf:array<int>) ->                    
                    for i in 0..3 do
                        let f =
                            let g = buf.[1] + 1
                            i + g
                        buf.[i] <- f
            @>

        let run,check = checkResult command
        run _1d intInArr        
        check intInArr [|2;3;6;7|]

    [<Test>]
    member this.``Template Let Transformation Test 19``() =
        let command = 
            <@ fun (range:_1D) (buf:array<int>) ->                    
                    for i in 0..3 do
                        let f x =
                            let g = buf.[1] + x
                            i + g
                        buf.[i] <- f 1
            @>

        let run,check = checkResult command
        run _1d intInArr        
        check intInArr [|2;3;6;7|]

    [<Test>]
    member this.``MatrixSum``() = 
        let command = 
            <@ 
                fun (range : _2D) rows (m1 : array<_>) (m2 : array<_>) (buf : array<_>) ->
                    let col = range.GlobalID0
                    let row = range.GlobalID1
                    buf.[row * rows + col] <- m1.[row * rows + col] + m2.[row * rows + col]                                         
            @>
        
        let run,check = checkResult command
        let m1 = [|[|1; 2; 3|]; [|4; 5; 6|]; [|7; 8; 9|]|]
        let m2 = [|[|9; 8; 7|]; [|6; 5; 4|]; [|3; 2; 1|]|]
        let buf = Array.zeroCreate 9
        let d = new _2D(3, 3, 1, 1)
        run d 3 (Array.concat m1) (Array.concat m2) buf
        check buf (Array.concat [|[|10; 10; 10|]; [|10; 10; 10|]; [|10; 10; 10|]|])

    [<Test>]
    member this.``ElemSum``() = 
        let command = 
            <@ 
                fun (range : _2D) rows (m : array<_>) (buf : array<_>) ->
                    let col = range.GlobalID0
                    let row = range.GlobalID1
                    buf.[0] <!+ m.[row * rows + col]                                        
            @>
        
        let run,check = checkResult command
        let m = [|[|1; 2; 3|]; [|4; 5; 6|]; [|7; 8; 9|]|]
        let buf = Array.zeroCreate 1
        let d = new _2D(3, 3, 1, 1)
        run d 3 (Array.concat m) buf
        check buf [|45|]

    [<Test>]
    member this.``matrixSum``() =
        let m1 = [|[|1; 2|]; [|3; 4|]|]
        let m2 = [|[|1; 2|]; [|3; 4|]|]
        Assert.AreEqual (sumMatr m1 m2, [|[|2; 4|]; [|6; 8|]|])

    [<Test>]
    member this.``matrixSum 2``() =
        let m1 = [|[|1; 2|]; [|3; 4|]|]
        let m2 = [|[|1; 2|]; [|3; 4|]|]
        let m3 = [|[|1; 2|]; [|3; 4|]|]
        let m4 = [|[|1; 2|]; [|3; 4|]|]
        let matrixArr = [|(m1, m2); (m3, m4)|]
        let sumArr = Array.map (fun (m1, m2) -> sumMatr m1 m2) matrixArr 
        Assert.AreEqual (sumArr, [|[|[|2; 4|]; [|6; 8|]|]; [|[|2; 4|]; [|6; 8|]|]|])
                
    [<Test>]
    member this.``matrixElSum``() =
        let m = [|[|1; 2; 3|]; [|1; 1; 1|]; [|4; 5; 3|]|]
        Assert.AreEqual (sumEl m, [|21|])

    [<Test>]
    member this.``matrixElSum 2``() =
        let matrixArr = [|[|[|1; 2|]; [|3; 4|]|]; [|[|5; 6|]; [|7; 8|]|]; [|[|9; 10|]; [|11; 12|]|]|]
        let sumArr = Array.map sumEl matrixArr
        Assert.AreEqual (sumArr, [|[|10|]; [|26|]; [|42|]|])

let x = 
    let d = ref 0
    fun y ->
        let r = !d
        d := !d + y
        r
