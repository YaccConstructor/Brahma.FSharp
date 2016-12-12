module MarshallingIssue

open Brahma.Helpers
open OpenCL.Net
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Extensions


let GetOutputMatrixDimensions aRows aCols bRows bCols =
    if aCols <> bRows
    then failwith "Cannot multiply these two matrices"
    aRows,bCols

let Multiply64 (a:array<_>) aRows aCols (b:array<_>) bRows bCols (c:array<_>) =
    let cRows, cCols = GetOutputMatrixDimensions aRows aCols bRows bCols
    for i in 0 .. cRows - 1 do
        for j in 0 .. cCols - 1 do
            let mutable buf = 0.0
            for k in 0 .. aCols - 1 do
                 buf <- buf + a.[i * aCols + k] * b.[k * bCols + j]
            c.[i * cCols + j] <- c.[i * cCols + j] + buf

let Multiply32 (a:array<_>) aRows aCols (b:array<_>) bRows bCols (c:array<_>) =
    let cRows, cCols = GetOutputMatrixDimensions aRows aCols bRows bCols
    for i in 0 .. cRows - 1 do
        for j in 0 .. cCols - 1 do
            let mutable buf = 0.0f
            for k in 0 .. aCols - 1 do
                 buf <- buf + a.[i * aCols + k] * b.[k * bCols + j]
            c.[i * cCols + j] <- c.[i * cCols + j] + buf

//fp64 version of the following programm:
//https://github.com/gsvgit/Brahma.FSharp/blob/master/Source/MatrixMultiply/MatrixMultiply.fs
//All references changed to float type, all console output removed
let Float64Version platformName (m1: array<_>) (m2: array<_>) =    

    let rows = 200
    let columns = 200
    let localWorkSize = 2
    let iterations = 10
    let deviceType = DeviceType.Default

    let provider =
        try  ComputeProvider.Create(platformName, deviceType)
        with 
        | ex -> failwith ex.Message

    let mutable commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)

    let aValues = m1
    let bValues = m2
    let cParallel = Array.zeroCreate(rows * columns)

    let command = 
        <@
            fun (r:_2D) (a:array<_>) (b:array<_>) (c:array<_>) -> 
                let tx = r.GlobalID0
                let ty = r.GlobalID1
                let mutable buf = c.[ty * columns + tx]
                for k in 0 .. columns - 1 do
                    buf <- buf + (a.[ty * columns + k] * b.[k * columns + tx])
                c.[ty * columns + tx] <- buf
        @>

    let cNormal = Array.zeroCreate (rows * columns)
    for i in 0 .. iterations - 1 do
        Multiply64 aValues rows columns bValues rows columns cNormal

    let kernel, kernelPrepare, kernelRun = provider.Compile command
    let d =(new _2D(rows, columns, localWorkSize, localWorkSize))
    kernelPrepare d aValues bValues cParallel
    
    for i in 0 .. iterations - 1 do
        commandQueue.Add(kernelRun()).Finish()|> ignore
        
    let _ = commandQueue.Add(cParallel.ToHost provider).Finish()
    
    let mutable isSuccess = true
    for i in 0 .. rows * columns - 1 do
        if isSuccess && System.Math.Abs(float (cParallel.[i] - cNormal.[i])) > 0.01
        then
            isSuccess <- false
            printfn "Expected: %A Actual: %A Error = %A" cNormal.[i] cParallel.[i] (System.Math.Abs(cParallel.[i] - cNormal.[i]))

    commandQueue.Dispose()
    provider.Dispose()
    provider.CloseAllBuffers()
    
    isSuccess

//fp32 version of the program
let Float32Version platformName (m1: array<_>) (m2: array<_>) =    

    let rows = 200
    let columns = 200
    let localWorkSize = 2
    let iterations = 10
    let deviceType = DeviceType.Default

    let provider =
        try  ComputeProvider.Create(platformName, deviceType)
        with 
        | ex -> failwith ex.Message

    let mutable commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)

    let aValues = m1
    let bValues = m2
    let cParallel = Array.zeroCreate(rows * columns)

    let command = 
        <@
            fun (r:_2D) (a:array<_>) (b:array<_>) (c:array<_>) -> 
                let tx = r.GlobalID0
                let ty = r.GlobalID1
                let mutable buf = c.[ty * columns + tx]
                for k in 0 .. columns - 1 do
                    buf <- buf + (a.[ty * columns + k] * b.[k * columns + tx])
                c.[ty * columns + tx] <- buf
        @>

    let cNormal = Array.zeroCreate (rows * columns)
    for i in 0 .. iterations - 1 do
        Multiply32 aValues rows columns bValues rows columns cNormal

    let kernel, kernelPrepare, kernelRun = provider.Compile command
    let d =(new _2D(rows, columns, localWorkSize, localWorkSize))
    kernelPrepare d aValues bValues cParallel
    
    for i in 0 .. iterations - 1 do
        commandQueue.Add(kernelRun()).Finish()|> ignore
        
    let _ = commandQueue.Add(cParallel.ToHost provider).Finish()
    
    let mutable isSuccess = true
    for i in 0 .. rows * columns - 1 do
        if isSuccess && System.Math.Abs(float32 (cParallel.[i] - cNormal.[i])) > 0.01f
        then
            isSuccess <- false
            printfn "Expected: %A Actual: %A Error = %A" cNormal.[i] cParallel.[i] (System.Math.Abs(cParallel.[i] - cNormal.[i]))            

    commandQueue.Dispose()
    provider.Dispose()
    provider.CloseAllBuffers()
    
    isSuccess
