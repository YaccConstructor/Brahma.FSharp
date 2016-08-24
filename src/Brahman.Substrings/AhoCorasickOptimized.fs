module Brahman.Substrings.AhoCorasick

open Brahma.Helpers
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Extensions
open Brahma.FSharp.OpenCL.Translator.Common
open System.Threading.Tasks

let label = "OpenCL/AhoCorasick"

let buildStateMachine templates maxTemplateLength (next:array<array<int16>>) (leaf:array<int16>) =
    let go = Array.init (templates * (int) maxTemplateLength * 256) (fun _ -> -1s)
    let link = Array.init (templates * (int) maxTemplateLength) (fun _ -> -1s)
    let exit = Array.init (templates * (int) maxTemplateLength) (fun _ -> -1s)
    let parent = Array.init (templates * (int) maxTemplateLength) (fun _ -> -1s)
    let parentChar = Array.init (templates * (int) maxTemplateLength) (fun _ -> 0uy)

    let rec goFunction v c =
        let rec getLink v =
            if link.[(int) v] < 0s then
                if v = 0s || parent.[(int) v] = 0s then
                    link.[(int) v] <- 0s
                else
                    link.[(int) v] <- goFunction (getLink parent.[(int) v]) parentChar.[(int) v]
            link.[(int) v]

        if go.[(int) v * 256 + (int) c] < 0s then
            if next.[(int) v].[(int) c] >= 0s then
                go.[(int) v * 256 + (int) c] <- next.[(int) v].[(int) c]
            else
                if v = 0s then
                    go.[(int) v * 256 + (int) c] <- 0s
                else
                    go.[(int) v * 256 + (int) c] <- goFunction (getLink v) c
        go.[(int) v * 256 + (int) c]

    let rec findParents v =
        for c in 0..255 do
            if next.[(int) v].[c] >= 0s then
                parent.[(int) next.[(int) v].[c]] <- v
                parentChar.[(int) next.[(int) v].[c]] <- (byte) c
                findParents next.[(int) v].[c]

    let rec processVertex v =
        for c in 0..255 do
            ignore (goFunction v ((byte) c))
            if next.[(int) v].[c] >= 0s then
                processVertex next.[(int) v].[c]

    let rec findExits v =
        let rec findExit v =
            match v with
            | 0s -> 0s
            | v when leaf.[(int) link.[(int) v]] >= 0s -> link.[(int) v]
            | v -> findExit link.[(int) v]

        for c in 0..255 do
            exit.[(int) v] <- findExit v
            if next.[(int) v].[c] >= 0s then
                findExits next.[(int) v].[c]

    findParents 0s
    processVertex 0s
    findExits 0s

    go, link, exit

let command = 
    <@
        fun (rng:_1D) l k templates (lengths:array<byte>) (go:array<int16>) (exit:array<int16>) (leaf:array<int16>) maxLength (input:array<byte>) (t:array<byte>) (result:array<int16>) ->
            let r = rng.GlobalID0
            let mutable _start = r * k
            let mutable _end = _start + k + (int) maxLength - 1
            if _end > l then _end <- l

            let localTemplateLengths = local (Array.zeroCreate 512)

            let groupSize = 512
            let chunk = (templates + groupSize - 1) / groupSize
            let id = rng.LocalID0

            let upperBound = (id + 1) * chunk
            let mutable higherIndex = upperBound - 1
            if upperBound > templates then
                higherIndex <- templates - 1

            for index in (id * chunk)..higherIndex do
                localTemplateLengths.[index] <- lengths.[index]

            for i in _start .. (_end - 1) do
                result.[i] <- -1s

            barrier()

            let mutable v = 0s
            for i in _start .. (_end - 1) do
                if i - _start = 65 then
                    barrier()

                v <- go.[256 * (int) v + (int) input.[i]]
                let mutable parent = v

                while parent > 0s do
                    let mutable currentTemplate = leaf.[(int) parent]
                    if currentTemplate >= 0s then
                        let position = i - (int) localTemplateLengths.[(int) currentTemplate] + 1
                        if result.[position] < currentTemplate then
                            result.[position] <- currentTemplate
                    parent <- exit.[(int) parent]
    @>