module Brahman.Substrings.NaiveSearch

open Brahma.Helpers
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Extensions
open Brahma.FSharp.OpenCL.Translator.Common
open System.Threading.Tasks

let label = "OpenCL/Naive"

let command = 
    <@
        fun (rng:_1D) l k templates (lengths:array<byte>) (input:array<_>) (t:array<byte>) (result:array<int16>) ->
            let r = rng.GlobalID0
            let _start = r * k
            let mutable _end = _start + k
            if _end > l then _end <- l
            for i in _start..(_end - 1) do
                result.[i] <- -1s
                let mutable templateBase = 0
                for n in 0..(templates - 1) do
                    if n > 0 then templateBase <- templateBase + (int) lengths.[n - 1]
                    let currentLength = lengths.[n]
                    if i + (int) currentLength <= l then
                        let mutable matches = 1
                        let mutable j = 0
                        while (matches = 1 && j < (int) currentLength) do
                            if input.[i + j] <> t.[templateBase + j] then matches <- 0
                            j <- j + 1

                        if matches = 1 then result.[i] <- (int16) n
    @>