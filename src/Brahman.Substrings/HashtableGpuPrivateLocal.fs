module Brahman.Substrings.Hashtables

open Brahma.Helpers
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Extensions
open Brahma.FSharp.OpenCL.Translator.Common
open System.Threading.Tasks

let label = "OpenCL/Hashtables"

let command = 
    <@
        fun (rng:_1D) l k templates (lengths:array<byte>) (hashes:array<byte>) (table:array<int16>) (next:array<int16>) (starts:array<int16>) maxLength (input:array<byte>) (t:array<byte>) (result:array<int16>) ->
            let r = rng.GlobalID0
            let mutable _start = r * k
            let mutable _end = _start + k
            if _end > l then _end <- l

            let privateHashes = Array.zeroCreate 32
            privateHashes.[0] <- input.[_start]
            for i in 1..((int) maxLength - 1) do
                if _start + i < l then privateHashes.[i] <- privateHashes.[i - 1] + input.[_start + i]

            let localTemplateHashes = local (Array.zeroCreate 512)
            let localTemplateLengths = local (Array.zeroCreate 512)
            let localTable = local (Array.zeroCreate 256)
            let localNext = local (Array.zeroCreate 512)
            let localStarts = local (Array.zeroCreate 512)

            let groupSize = 512
            let chunk = (templates + groupSize - 1) / groupSize
            let tableChunk = (256 + groupSize - 1) / groupSize

            let id = rng.LocalID0

            let upperBound = (id + 1) * chunk
            let mutable higherIndex = upperBound - 1
            if upperBound > templates then
                higherIndex <- templates - 1

            let upperBoundTable = (id + 1) * tableChunk
            let mutable higherIndexTable = upperBoundTable - 1
            if upperBoundTable > 256 then
                higherIndexTable <- 256 - 1

            for index in (id * chunk)..higherIndex do
                localTemplateHashes.[index] <- hashes.[index]
                localTemplateLengths.[index] <- lengths.[index]
                localNext.[index] <- next.[index]
                localStarts.[index] <- starts.[index]

            for index in (id * tableChunk)..higherIndexTable do
                localTable.[index] <- table.[index]

            barrier()

            for i in _start .. (_end - 1) do
                result.[i] <- -1s
                if i > _start then
                    for current in 0..((int) maxLength - 2) do
                        privateHashes.[current] <- privateHashes.[current + 1] - input.[i - 1]
                    if i + (int) maxLength <= l then
                        privateHashes.[(int) maxLength - 1] <- privateHashes.[(int) maxLength - 2] + input.[i + (int) maxLength - 1]
                
                let mutable maximum = (int) maxLength - 1
                if i + maximum >= l then
                    maximum <- l - i - 1
                for current in 0..maximum do
                    let hash = privateHashes.[(int) current]
                    let number = localTable.[(int) hash]
                    let mutable templateIndex = number
                    let mutable look = 1
                    while look > 0 && templateIndex >= 0s do
                        if localTemplateLengths.[(int) templateIndex] = (byte) current + 1uy then
                            let mutable matches = 1

                            let mutable j = 0
                            let templateBase = localStarts.[(int) templateIndex]
                            while (matches = 1 && j < current + 1) do
                                if input.[i + j] <> t.[(int) templateBase + j] then  matches <- 0
                                j <- j + 1

                            if matches = 1 then
                                result.[i] <- templateIndex
                                look <- 0
                        templateIndex <- localNext.[(int) templateIndex]
    @>


let createHashTable templates (templateLengths:array<byte>) (templateHashes:array<byte>) =
    let table = Array.init 256 (fun _ -> -1s)
    let next = Array.init templates (fun _ -> -1s)

    for i in 0..(templates - 1) do
        let mutable current = table.[(int) templateHashes.[i]]
        
        if current = -1s then
            table.[(int) templateHashes.[i]] <- (int16) i
        else
            while next.[(int) current] >= 0s do
                current <- next.[(int) current]

            next.[(int) current] <- (int16) i

    table, next
        
let computeTemplateStarts templates (templateLengths:array<byte>) =
    let starts = Array.zeroCreate templates
    let mutable current = 0s

    for i in 0..(templates - 1) do
        starts.[i] <- current
        current <- current + (int16) templateLengths.[i]
    
    starts