module Brahman.Substrings.Helpers

open System.Collections.Generic
open Brahma.Helpers
open Brahma.OpenCL

let computeTemplateHashes templates templatesSum (templateLengths:array<byte>) (templateArr:array<byte>) =
    let hashes = Array.zeroCreate templates
    let mutable templateBase = 0
    for n in 0..(templates - 1) do
        if n > 0 then templateBase <- templateBase + (int) templateLengths.[n - 1]
        let templateEnd = templateBase + (int) templateLengths.[n]
        for i in templateBase..(templateEnd - 1) do
            hashes.[n] <- hashes.[n] + templateArr.[i]
    hashes

let random = new System.Random()    

let computeTemplatesSum templates (templateLengths:array<byte>) =
    let mutable l = 0
    for i in 0..(templates - 1) do
        l <- l + (int) templateLengths.[i]
    l 

let computeTemplateLengths templates maxTemplateLength =
    Array.sort (Array.init templates (fun _ -> (byte) (random.Next((int) maxTemplateLength - (int) 1us) + (int) 1us)))

let buildSyntaxTree templates maxTemplateLength (templateLengths:array<byte>) (templateArr:array<byte>) =
    let next = Array.init (templates * (int) maxTemplateLength) (fun _ -> Array.init 256 (fun _ -> -1s))
    let leaf = Array.init (templates * (int) maxTemplateLength) (fun _ -> -1s)
    let prefix = Array.init (templates * (int) maxTemplateLength) (fun _ -> -1s)

    let mutable vertices = 0s
    let mutable templateBase = 0

    for n in 0..(templates - 1) do
        let mutable v = 0
        for i in 0..((int) templateLengths.[n] - 1) do
            if next.[v].[(int) templateArr.[templateBase + i]] < 0s then
                vertices <- vertices + 1s
                next.[v].[(int) templateArr.[templateBase + i]] <- vertices
            v <- (int) next.[v].[(int) templateArr.[templateBase + i]]

            if leaf.[v] >= 0s then
                prefix.[n] <- leaf.[v]
        leaf.[v] <- (int16) n
        templateBase <- templateBase + (int) templateLengths.[n]

    prefix, next, leaf, vertices


let findPrefixes templates maxTemplateLength (templateLengths:array<byte>) (templateArr:array<byte>) =
    let prefix, _, _, _ = buildSyntaxTree templates maxTemplateLength templateLengths templateArr

    prefix

let countMatches (result:array<int16>) maxTemplateLength bound length (templateLengths:array<byte>) (prefix:array<int16>) =
    let mutable matches = 0
    let clearBound = min (bound - 1) (length - (int) maxTemplateLength)

    for i in 0..clearBound do
        let mutable matchIndex = result.[i]
        if matchIndex >= 0s then
            matches <- matches + 1

    for i in (clearBound + 1)..(bound - 1) do
        let mutable matchIndex = result.[i]
        while matchIndex >= 0s && i + (int) templateLengths.[(int) matchIndex] > length do
            matchIndex <- prefix.[(int) matchIndex]
            
        if matchIndex >= 0s then
            matches <- matches + 1
    matches

let filterMatches (result:array<int16>) (maxTemplateLength:byte) bound length (templateLengths:array<byte>) (prefix:array<int16>) (dictionary:Dictionary<int, int16>) =
    let mutable matches = 0
    let clearBound = min (bound - 1) (length - (int) maxTemplateLength)

    for i in 0..clearBound do
        let mutable matchIndex = result.[i]
        if matchIndex >= 0s then
            dictionary.Add(i, matchIndex)
            matches <- matches + 1

    for i in (clearBound + 1)..(bound - 1) do
        let mutable matchIndex = result.[i]
        while matchIndex >= 0s && i + (int) templateLengths.[(int) matchIndex] > length do
            matchIndex <- prefix.[(int) matchIndex]
            
        if matchIndex >= 0s then
            dictionary.Add(i, matchIndex)
            matches <- matches + 1
    matches

let verifyResults (expected:int) (actual:int) label =
    printfn "Verifying %A results..." label
    if System.Math.Abs(expected - actual) > 0 then        
        printfn "Expected: %A Actual: %A" expected actual
    else
        printfn "Found: %A" actual
    printfn "done."

let printTime (timer:Timer<string>) label =
    if timer <> null then
        printfn "Total time, %A: %A" label (timer.Total(label))

let printGlobalTime = printTime (Timer<string>.Global)

let printTotalTime (timer:Timer<string>) (readingTimer:Timer<string>) label =
    if timer <> null then
        printfn "Total time, %A: %A" label (timer.Total(label) + readingTimer.Total("reading"))

let chunk overlapSize (s:seq<'a>) =
    let firstChunk = ref true
    let overlappedBuf:array<'a> = Array.zeroCreate overlapSize
    let sEnum = s.GetEnumerator()
    let isLast = ref false
    let lastChunkS = ref 0
    fun (buf:array<_>) ->
        let l = buf.Length
        let mutable i = if !firstChunk then 0 else overlapSize
        while i < l do
            if sEnum.MoveNext()
            then
                let b = sEnum.Current
                buf.[i] <- b
            elif not !isLast
            then 
                isLast := true
                lastChunkS := i
            i <- i + 1
        if not !firstChunk
        then array.Copy(overlappedBuf,buf,overlappedBuf.Length)
        array.Copy(buf, l-overlapSize, overlappedBuf, 0, overlappedBuf.Length)
        firstChunk := false
        if !isLast then Some !lastChunkS else None

let compressResults =
    <@
        fun (rng:_1D) (counter:array<int>) (inArr:array<int16>) (result:array<byte>) ->
            let r = rng.GlobalID0
            if inArr.[r] > -1s
            then 
                let i = counter.[0] <!+> 1
                result.[i] <- byte inArr.[r]
    @>
