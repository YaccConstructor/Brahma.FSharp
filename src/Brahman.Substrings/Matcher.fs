module Brahman.Substrings.Matcher

open Brahma.Helpers
open Brahma.OpenCL
open Brahma.FSharp
open Brahma.FSharp.OpenCL.Core
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Extensions
open Brahma.FSharp.OpenCL.Translator.Common
open System.Threading.Tasks
open Microsoft.FSharp.Collections

type Config =
    {
        additionalArgs: uint64
        additionalTempData: uint64
        localWorkSize: int
        chunkSize: int
        groups: int
        groupSize: int
        bufLength: int
    }

type Templates = {
    number : int
    sizes : byte[]
    content : byte[]
    }

[<Struct>]
type MatchRes =
    val ChunkNum: int
    val Offset: int
    val PatternId : int
    new (chunkNum,offset,patternId) = {ChunkNum = chunkNum; Offset = offset; PatternId = patternId }

[<Struct>]
type FindRes =
    val Data: array<MatchRes>
    val Templates: array<array<byte>>
    val ChunkSize: int
    new (data,templates,chunkSize) = {Data = data; Templates = templates; ChunkSize = chunkSize }

//GPGPU provider
let platformName = "NVIDIA*"
let deviceType = OpenCL.Net.DeviceType.Default        

// Main
type Matcher(?maxHostMem) =    
    let totalResult = new ResizeArray<_>()
    let mutable label = ""
    
    let timer = new Timer<string>()

    let maxTemplateLength = 32       
    
    let mutable ready = true

    let maxHostMemory = match maxHostMem with Some x -> x | _ -> 256UL * 1024UL * 1024UL

    let configure (templates:array<_>) (provider:ComputeProvider) =
        let memory,ex = OpenCL.Net.Cl.GetDeviceInfo(provider.Devices |> Seq.head, OpenCL.Net.DeviceInfo.MaxMemAllocSize)
        let maxGpuMemory = memory.CastTo<uint64>()

        let tLenghth = templates |> Array.length |> uint64
        let additionalArgs = 2UL * (256UL + 2UL) * (uint64) maxTemplateLength * tLenghth + tLenghth +
                             tLenghth + 13UL + 1000UL

        let additionalTempData = 2UL * (256UL + 256UL + 3UL) * (uint64) maxTemplateLength * tLenghth + 
                                 (uint64) maxTemplateLength * tLenghth + 100000UL

        let availableMemory = (int) (min (maxGpuMemory - additionalArgs) (maxHostMemory - additionalArgs - additionalTempData))
        let lws,ex = OpenCL.Net.Cl.GetDeviceInfo(provider.Devices |> Seq.head, OpenCL.Net.DeviceInfo.MaxWorkGroupSize)
        let localWorkSize = int <| lws.CastTo<uint64>()
        let chunkSize = 256
        let groupSize = chunkSize * localWorkSize * (1 + 2)
        let groups = availableMemory / groupSize
        let length = chunkSize * localWorkSize * groups
        {
            additionalArgs = additionalArgs
            additionalTempData = additionalTempData
            localWorkSize = localWorkSize
            chunkSize = chunkSize
            groups = groups
            groupSize = groupSize
            bufLength = length
        }

    let readingTimer = new Timer<string>()
    let countingTimer = new Timer<string>()   

    let close (provider:ComputeProvider) = 
        provider.CloseAllBuffers()        

    let countMatchesDetailed index (result:array<uint16>) maxTemplateLength bound length (templateLengths:array<byte>) (prefix:array<int16>) (matchesArray:array<uint64>) offset =
        let mutable matches = 0
        let clearBound = min (!bound - 1) (length - (int) maxTemplateLength)        
        let mutable resultOffset = 0
        while resultOffset <= offset - 3  do
            let i = int ((uint32 result.[resultOffset] <<< 16) ||| uint32 result.[resultOffset+1])
            let mutable matchIndex = result.[resultOffset+2]                            
            if 0 < i && i < clearBound
            then
                matchesArray.[(int) matchIndex] <- matchesArray.[(int) matchIndex] + 1UL
                totalResult.Add(new MatchRes(index, i, int matchIndex))
                matches <- matches + 1
            else           
                while matchIndex >= 0us && i + (int) templateLengths.[(int) matchIndex] > length do
                    matchIndex <- uint16 prefix.[(int) matchIndex]                                            
                matchesArray.[(int) matchIndex] <- matchesArray.[(int) matchIndex] + 1UL
                totalResult.Add(new MatchRes(index, i, int matchIndex))
                matches <- matches + 1
            resultOffset <- resultOffset + 3

        matches

    let printResult (templates:Templates) (matches:array<_>) counter =
        let hex = Array.map (fun (x : byte) -> System.String.Format("{0:X2} ", x)) templates.content
        let mutable start = 0
        for i in 0..(templates.number - 1) do
            let pattern = System.String.Concat(Array.sub hex start ((int) templates.sizes.[i]))
            printfn "%A: %A matches found by %A" pattern matches.[i] label
            start <- start + (int) templates.sizes.[i]

        printfn ""

        printfn "Total found by %A: %A" label counter

    let prepareTemplates array = 
        let sorted = Array.sortBy (fun (a:byte[]) -> a.Length) array
        let lengths = Array.map (fun (a:byte[]) -> (byte) a.Length) sorted
        let templateBytes = Array.toSeq sorted |> Array.concat
        let readyTemplates = { number = sorted.Length; sizes = lengths; content = templateBytes;}
        readyTemplates

    let finalize provider =
        close provider

    let sorted (templates:Templates) = 
        let start = ref 0
        [|for i in 0..(templates.number - 1) do
            let pattern = Array.sub templates.content !start ((int) templates.sizes.[i])
            start := !start + (int) templates.sizes.[i]
            yield pattern
        |]

    let chankSize = ref 0

    let rk readFun templateArr = 
        let counter = ref 0
        readingTimer.Start()
        
        let matches = Array.zeroCreate 512
                
        let  countingBound,matchBound = ref 0, ref 0
        let  index,totalIndex,current = ref 0, ref 0, ref 0L
        let isLastChunk = ref false
        

        label <- RabinKarp.label
        
        let templates = prepareTemplates templateArr
        
        let prefix, next, leaf, _ = Helpers.buildSyntaxTree templates.number (int maxTemplateLength) templates.sizes templates.content        
        let templateHashes = Helpers.computeTemplateHashes templates.number templates.content.Length templates.sizes templates.content                

        totalResult.Clear()

        let providers = new ResizeArray<_>()

        let bufs = new ResizeArray<_>()        

        let postprocess = fun (result,c) ->
            countingBound := !chankSize
            matchBound := !chankSize
            incr index
            incr totalIndex
            current := !current + (int64) !chankSize
            counter := 
                !counter 
                + countMatchesDetailed (!totalIndex-1) result maxTemplateLength countingBound !matchBound templates.sizes prefix matches c
            
            if !index = 50 then
                printfn "I am %A and I've already read %A bytes!" label !current
                index := 0

        let createWorkerFun (wConfig:Agents.WorkerConfig) =
            
            let c = [|0|]            
            let config = configure templateArr wConfig.GPUProvider

            let l = (config.bufLength + (config.chunkSize-1))/config.chunkSize 
            let d = new _1D(l,config.localWorkSize)

            let result = Array.zeroCreate config.bufLength
            let input = Array.zeroCreate config.bufLength 

            chankSize := config.bufLength

            bufs.Add input
            for _ in 0 .. int wConfig.AdditionalBuffsNum do
                bufs.Add <| Array.zeroCreate config.bufLength
                        
            let kernel, kernelPrepare, kernelRun = wConfig.GPUProvider.Compile(query=RabinKarp.command, translatorOptions=[BoolAsBit])                
            kernelPrepare
                d config.bufLength config.chunkSize templates.number templates.sizes templateHashes maxTemplateLength input templates.content result c                

            let f = fun data ->
                ignore <| wConfig.GpuCommandQueue.Add(c.ToGpu(wConfig.GPUProvider, [|0|]))
                ignore <| wConfig.GpuCommandQueue.Add(input.ToGpu(wConfig.GPUProvider, data))
                ignore <| wConfig.GpuCommandQueue.Add(kernelRun())
                ignore <| wConfig.GpuCommandQueue.Add(result.ToHost wConfig.GPUProvider).Finish()                
                ignore <| wConfig.GpuCommandQueue.Add(c.ToHost wConfig.GPUProvider).Finish()
                result,c.[0]
                
            f   
        
        let workers () =             
            Array.init 2 
                (fun i ->
                    let provider =
                        try  ComputeProvider.Create(platformName, deviceType)
                        with 
                        | ex -> failwith ex.Message
                    provider |> printfn "%A"
                    providers.Add provider
                    let commandQueue = new CommandQueue(provider, provider.Devices |> Seq.item 0) 
                    let f = new Agents.WorkerConfig(1u,commandQueue,provider) |> createWorkerFun
                    new Agents.Worker<_,_>(f))

        let start = System.DateTime.Now
        let ws = workers ()
        let master = new Agents.Master<_,_,_>(ws, readFun, bufs, Some postprocess)
        while not <| master.IsDataEnd() do ()        
        master.Die()
        printfn "Total time = %A " (System.DateTime.Now - start)
        providers |> ResizeArray.iter finalize         
        
        new FindRes(totalResult.ToArray(), sorted templates, !chankSize )

    new () = Matcher (256UL * 1024UL * 1024UL)

    member this.RabinKarp (readFun, templateArr) = 
        rk readFun templateArr

    member this.RabinKarp (hdId, templateArr) =         
        let handle = RawIO.CreateFileW hdId        
        let res = rk (RawIO.ReadHD handle) templateArr
        RawIO.CloseHandle(handle)
        |> ignore
        res

    member this.RabinKarp (inSeq, templateArr) = 
        let readF = 
            let next = Helpers.chunk 32 inSeq
            let finish = ref false
            fun buf ->
                if !finish
                then None
                else
                    let r = next buf
                    match r with
                    | None -> ()
                    | Some x -> finish := true
                    Some buf

        rk readF templateArr

    member this.InBufSize with get () = !chankSize