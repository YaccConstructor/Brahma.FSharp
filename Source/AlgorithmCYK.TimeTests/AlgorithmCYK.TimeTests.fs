module AlgorithmCYK.TimeTests

open AlgorithmCYK
open AlgorithmCYK.ParallelCPU
open AlgorithmCYK.ParallelGPGPU

open FSharp.Charting
open System.Drawing
open System.Windows.Forms

let stringGener (symb : array<string>) n = 
    let len = symb.Length
    let rnd = System.Random()
    let mutable str = ""
    for i = 0 to n - 1 do
        str <- str + symb.[rnd.Next() % len]
    str

let (rl1, start1) = ([|("S","AB"); ("S","BC"); ("A","BA"); ("B","CC"); ("C","AB"); ("B","b"); ("A","a"); ("C","a")|], "S")
let nonterm1 = [|"a";"b"|]

let time f =
    let start = System.DateTime.Now
    for i = 1 to 5 do f()
    (System.DateTime.Now - start).TotalMilliseconds / 5.00

let charting =
    Chart.Combine
        [
             Chart.Line( [ for i in 0..2..200 -> (i, time(fun () -> CYK rl1 (stringGener nonterm1 i) "S" |> ignore))], "CYK", Color = System.Drawing.Color.Green)
             Chart.Line( [ for i in 0..5..200 -> (i, time(fun () -> CYKParallelCPU rl1 (stringGener nonterm1 i) "S" |> ignore))], "CYKParallelCPU", Color = System.Drawing.Color.Crimson)
             Chart.Line( [ for i in 0..2..20 -> (i, time(fun () -> CYKParallelGPGPU rl1 (stringGener nonterm1 i) "S" |> ignore))], "CYKParallelGPGPU", Color = System.Drawing.Color.Aqua)
        ]

do System.Windows.Forms.Application.Run(charting.ShowChart())
