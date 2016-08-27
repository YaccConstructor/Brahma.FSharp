module Brahman.Substrings.RabinKarp

open Brahma.Helpers
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Extensions
open Brahma.FSharp.OpenCL.Translator.Common
open System.Threading.Tasks

let label = "OpenCL/RabinKarp"

let command = 
    <@
        fun (rng:_1D) l k templates (lengths:array<byte>) (hashes:array<byte>) maxLength (input:array<byte>) (t:array<byte>) (result:array<uint16>) (resLenth:array<int>)->
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

            let groupSize = 512
            let chunk = (512 + groupSize - 1) / groupSize
            let id = rng.LocalID0

            let upperBound = (id + 1) * chunk
            let mutable higherIndex = upperBound - 1
            if upperBound > 512 then
                higherIndex <- 512 - 1

            for index in (id * chunk)..higherIndex do
                localTemplateHashes.[index] <- hashes.[index]
                localTemplateLengths.[index] <- lengths.[index]

            barrier()

            for i in _start .. (_end - 1) do
                //result.[i] <- -1s
                if i > _start then
                    for current in 0..((int) maxLength - 2) do
                        privateHashes.[current] <- privateHashes.[current + 1] - input.[i - 1]
                    if i + (int) maxLength <= l then
                        privateHashes.[(int) maxLength - 1] <- privateHashes.[(int) maxLength - 2] + input.[i + (int) maxLength - 1]

                let mutable templateBase = 0
                for n in 0..(templates - 1) do
                    if n > 0 then templateBase <- templateBase + (int) localTemplateLengths.[n - 1]
                        
                    let currentLength = (int) localTemplateLengths.[n]
                    if (i + (int) currentLength) <= l && localTemplateHashes.[n] = privateHashes.[currentLength - 1] then
                        let mutable matches = 1

                        let mutable j = 0
                        while (matches = 1 && j < (int) currentLength) do
                            if input.[i + j] <> t.[templateBase + j] then  matches <- 0
                            j <- j + 1

                        if matches = 1
                        then
                            let j = resLenth.[0] <!+> 3 
                            result.[j] <- uint16 (uint32(i >>> 16) &&& 0xFFFFFFFFu)
                            result.[j+1] <- uint16 (uint32 i &&& 0xFFFFFFFFu)
                            result.[j+2] <- (uint16) n
    @>