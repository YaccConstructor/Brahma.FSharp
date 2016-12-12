module FloatingPointExamples

open Brahma.Helpers
open OpenCL.Net
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Extensions

//The following function performs transition from Cartesian coordinates to sperical
//Array 'cart' contains Cartesian coordinates:
//cart.[0] — x; cart.[1] — y; cart.[2] — z
//Output array 'spherical' contains sperical coordinates:
//spherical.[0] — radial
//spherical.[1] — polar
//spherical.[2] — azimuthal
let CartesianToSpherical (cart: array<float>) =
    let localWorkSize = 1
    let platformName = "NVIDIA*"
    let deviceType = DeviceType.Default
    let length = 3

    let provider =
        try  ComputeProvider.Create(platformName, deviceType)
        with 
        | ex -> failwith ex.Message
   
    let commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)

    let command  =
        <@ fun (rng:_1D) (a: array<float>) (s: array<float>)->
            let r = sqrt(a.[0] * a.[0] + a.[1] * a.[1] + a.[2] * a.[2])
            s.[0] <- r
            s.[1] <- acos(a.[2] / r)
            s.[2] <- atan(a.[1] / a.[0])
        @>

    let kernel, kernelPrepare, kernelRun = provider.Compile command
    let d =(new _1D(length, localWorkSize))
    
    let spherical = Array.zeroCreate 3

    kernelPrepare d cart spherical

    let _ = commandQueue.Add(kernelRun())
    let _ = commandQueue.Add(spherical.ToHost provider).Finish()
    commandQueue.Dispose()
    provider.Dispose()
    provider.CloseAllBuffers()

    spherical


//Following function calculates the volume of the pyramid ABCD.
//Array 'arr' contains the length of the edges:
// arr.[0] = |AB|
// arr.[1] = |BC|
// arr.[2] = |AC|
// arr.[3] = |AD|
// arr.[4] = |BD|
// arr.[5] = |CD|
//The last element of the array contains volume
let PyramidVolume (arr: array<float>) =
    let localWorkSize = 1
    let platformName = "NVIDIA*"
    let deviceType = DeviceType.Default
    let length = 6

    if arr.[0] + arr.[1] < arr.[2]
    then failwith "It's not a pyramid. Face ABC doesn't exist."
    elif arr.[0] + arr.[1] < arr.[4]
    then failwith "It's not a pyramid. Face ACD doesn't exist."
    elif arr.[0] + arr.[2] < arr.[5]
    then failwith "It's not a pyramid. Face BCD doesn't exist."
    elif(arr.[0] + arr.[1] < arr.[3])
    then failwith "It's not a pyramid. Face ABD doesn't exist."
    
    let provider =
        try  ComputeProvider.Create(platformName, deviceType)
        with 
        | ex -> failwith ex.Message
   
    let commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)

    let command  =
        <@ fun (rng:_1D) (s: array<float>) ->
            let area a b c =
                let p = (a + b + c) / 2.0
                sqrt(p * (p - a) * (p - b) * (p - c))
            let s1 = area s.[0] s.[1] s.[2]
            let s2 = area s.[1] s.[4] s.[5]
            let h1 = 2.0 * s1 / s.[1]
            let h2 = 2.0 * s2 / s.[1]
            let l = area s.[3] h1 h2
            let h = l * 2.0 / h1
            s.[6] <- (area s.[0] s.[1] s.[2]) * h / 3.0
        @>
        
    let kernel, kernelPrepare, kernelRun = provider.Compile command
    let d =(new _1D(length, localWorkSize))

    let res = arr
    
    kernelPrepare d res

    let _ = commandQueue.Add(kernelRun())
    let _ = commandQueue.Add(res.ToHost provider).Finish()
    commandQueue.Dispose()
    provider.Dispose()
    provider.CloseAllBuffers()

    res

//Following function executes LaGrange interpolation. Array 'interpolUnits' contains interpolation inits
//Array 'funcValues' contains values of function f(a)
//Array considered to be sorted except for the last elements. 
//interpolUnits.[length - 1] contains the interpolation unit for which the function value is calculated.
//The result of this calculation is stored in f.[length - 1]
let LaGrangeInterpol (interpolUnits: array<float>) (funcValues: array<float>) length =
    let localWorkSize = 1
    let platformName = "NVIDIA*"
    let deviceType = DeviceType.Default

    if (interpolUnits.[length - 1] < interpolUnits.[0]) || (interpolUnits.[length - 1] > interpolUnits.[length - 2])
    then failwith "invalid range"

    let provider =
        try  ComputeProvider.Create(platformName, deviceType)
        with 
        | ex -> failwith ex.Message
   
    let commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)

    let command  =
        <@ fun (rng:_1D) (x: array<float>) (y: array<float>) l ->
            for i in 0 .. l - 2 do
                let mutable L = 1.0
                for j in 0 .. i - 1 do
                    L <- L * (x.[l - 1] - x.[j]) / (x.[i] - x.[j])
                for j in i + 1 .. l - 2 do
                    L <- L * (x.[l - 1] - x.[j]) / (x.[i] - x.[j])
                y.[l - 1] <- y.[l - 1] + y.[i] * L
        @>
        
    let kernel, kernelPrepare, kernelRun = provider.Compile command
    let d =(new _1D(length, localWorkSize))
    
    let arr1 = interpolUnits
    let arr2 = funcValues
    
    kernelPrepare d arr1 arr2 length

    let _ = commandQueue.Add(kernelRun())
    let _ = commandQueue.Add(arr2.ToHost provider).Finish()
    commandQueue.Dispose()
    provider.Dispose()
    provider.CloseAllBuffers()

    arr2