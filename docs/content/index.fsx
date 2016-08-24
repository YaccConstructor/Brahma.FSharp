(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Brahma.FSharp
======================

Documentation

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      The Brahma.FSharp library can be <a href="https://nuget.org/packages/Brahma.FSharp">installed from NuGet</a>:
      <pre>PM> Install-Package Brahma.FSharp</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>

Example
-------

This example demonstrates using a function defined in this library.

*)

module MatrixMultiply

open Brahma.Helpers
open OpenCL.Net
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Extensions

let random = new System.Random()
        
let MakeMatrix rows cols =
    Array.init (rows * cols) (fun i -> float32 (random.NextDouble()))

let GetOutputMatrixDimensions aRows aCols bRows bCols =
    if aCols <> bRows
    then failwith "Cannot multiply these two matrices"
    aRows,bCols

let Multiply (a:array<_>) aRows aCols (b:array<_>) bRows bCols (c:array<_>) =
    let cRows, cCols = GetOutputMatrixDimensions aRows aCols bRows bCols
    for i in 0 .. cRows - 1 do
        for j in 0 .. cCols - 1 do
            let mutable buf = 0.0f
            for k in 0 .. aCols - 1 do
                 buf <- buf + a.[i * aCols + k] * b.[k * bCols + j]
            c.[i * cCols + j] <- c.[i * cCols + j] + buf
    
let Main platformName (m1: array<_>) (m2: array<_>) =    

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

    printfn "Multiplying two %Ax%A matrices %A times using .NET..." rows columns iterations
    let cNormal = Array.zeroCreate (rows * columns)
    for i in 0 .. iterations - 1 do
        Timer<string>.Global.Start()
        Multiply aValues rows columns bValues rows columns cNormal
        Timer<string>.Global.Lap(".NET")

    printfn "done."

    printfn 
        "Multiplying two %Ax%A matrices %A times using OpenCL and selected platform/device : %A ..." 
        rows columns iterations provider

    let kernel, kernelPrepare, kernelRun = provider.Compile command
    let d =(new _2D(rows, columns, localWorkSize, localWorkSize))
    kernelPrepare d aValues bValues cParallel
    
    for i in 0 .. iterations - 1 do
        Timer<string>.Global.Start()
        let _ = commandQueue.Add(kernelRun()).Finish()            
        Timer<string>.Global.Lap("OpenCL")
        
    let _ = commandQueue.Add(cParallel.ToHost provider).Finish()
    
    printfn "Verifying results..."
    let mutable isSuccess = true
    for i in 0 .. rows * columns - 1 do
        if isSuccess && System.Math.Abs(float32 (cParallel.[i] - cNormal.[i])) > 0.01f
        then
            isSuccess <- false
            printfn 
                "Expected: %A Actual: %A Error = %A" 
                cNormal.[i] cParallel.[i] (System.Math.Abs(cParallel.[i] - cNormal.[i]))            
            
    printfn "done."

    Timer<string>.Global.Average(".NET") |> printfn "Avg. time, F#: %A"
    Timer<string>.Global.Average("OpenCL") |> printfn "Avg. time, OpenCL: %A"

    commandQueue.Dispose()
    provider.Dispose()
    provider.CloseAllBuffers()
            
Main "NVIDIA*" (MakeMatrix 200 200) (MakeMatrix 200 200) |> ignore

(**

###Note

Sometimes calculations could be interrupted buy GPU driver (OS) timeout (TDR). 
For hot fix you can set TdrLevel registry key (KeyPath : HKEY\_LOCAL\_MACHINE\System\CurrentControlSet\Control\GraphicsDrivers) value to 0. 
If this key is not exists, then you should crete it. For more details look at ["TDR Registry Keys (Windows Drivers)"][tdr].

Samples & documentation
-----------------------

 * [Tutorial](tutorial.html) contains a further explanation of this sample library.

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the
   functions.
 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding a new public API, please also 
consider adding [samples][content] that can be turned into a documentation. You might
also want to read the [library design notes][readme] to understand how it works.

The library is available under Eclipse Public License, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/YaccConstructor/Brahma.FSharp/tree/master/docs/content
  [gh]: https://github.com/YaccConstructor/Brahma.FSharp
  [issues]: https://github.com/YaccConstructor/Brahma.FSharp/issues
  [readme]: https://github.com/YaccConstructor/Brahma.FSharp/blob/master/README.md
  [license]: https://github.com/YaccConstructor/Brahma.FSharp/blob/master/LICENSE.txt
  [tdr]: https://msdn.microsoft.com/en-us/library/windows/hardware/ff569918(v=vs.85).aspx
*)
