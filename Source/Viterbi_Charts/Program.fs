module Viterbi.Charts

open FSharp.Charting
open Viterbi.Cons
open Viterbi.Parallel
open Viterbi.GPGPU
open HMM.Viterbi.Tests

let getArLn ln = [|for i in 0..ln - 1 -> if i <> ln - 1 then "A" else "end"|]
let times = 2

let algImplTime fn times =
    let start = System.DateTime.Now
    for i in 0..(times - 1) do fn ()
    (System.DateTime.Now - start).TotalMilliseconds / (float)times

let getArgs1 i fn =
    let observSpace = RF02468.observSpace
    let stateSpace = RF02468.stateSpace
    let startProbs = RF02468.startProbs
    let transitionProbs = RF02468.transitionProbs
    let emissionProbs = RF02468.emissionProbs
    let observSeq = getArLn i
    fn [|0..observSpace.Length - 1|] stateSpace.Length startProbs [|for i in observSeq -> Array.findIndex ((=)i) observSpace|] transitionProbs emissionProbs

(getArgs1 10 Viterbi.Cons.viterbi) |> ignore
(getArgs1 10 Viterbi.Parallel.viterbi) |> ignore
(getArgs1 10 Viterbi.GPGPU.viterbi) |> ignore

let gch1 =     
    Chart.Combine
     [
      Chart.Line( [ for i in 20..5..140 -> (i, algImplTime (fun () -> (getArgs1 i Viterbi.Cons.viterbi) |> ignore) times ) ], "Cons68", Color = System.Drawing.Color.Red)
      Chart.Line( [ for i in 20..5..140 -> (i, algImplTime (fun () -> (getArgs1 i Viterbi.Parallel.viterbi) |> ignore) times )  ], "CPU68", Color = System.Drawing.Color.Green)
      Chart.Line( [ for i in 20..5..140 -> (i, algImplTime (fun () -> (getArgs1 i Viterbi.GPGPU.viterbi) |> ignore) times ) ], "GPGPU68", Color = System.Drawing.Color.Black)
     ]

do System.Windows.Forms.Application.Run(gch1.ShowChart())

let getArgs2 i fn =
    let observSpace = RF01123.observSpace
    let stateSpace = RF01123.stateSpace
    let startProbs = RF01123.startProbs
    let transitionProbs = RF01123.transitionProbs
    let emissionProbs = RF01123.emissionProbs
    let observSeq = getArLn i
    fn [|0..observSpace.Length - 1|] stateSpace.Length startProbs [|for i in observSeq -> Array.findIndex ((=)i) observSpace|] transitionProbs emissionProbs

(getArgs2 10 Viterbi.Cons.viterbi) |> ignore
(getArgs2 10 Viterbi.Parallel.viterbi) |> ignore
(getArgs2 10 Viterbi.GPGPU.viterbi) |> ignore

let gch2 =     
    Chart.Combine
     [
      Chart.Line( [ for i in 20..5..140 -> (i, algImplTime (fun () -> (getArgs2 i Viterbi.Cons.viterbi) |> ignore) times ) ], "Cons23", Color = System.Drawing.Color.Red)
      Chart.Line( [ for i in 20..5..140 -> (i, algImplTime (fun () -> (getArgs2 i Viterbi.Parallel.viterbi) |> ignore) times ) ], "CPU23", Color = System.Drawing.Color.Green)
      Chart.Line( [ for i in 20..5..140 -> (i, algImplTime (fun () -> (getArgs2 i Viterbi.GPGPU.viterbi) |> ignore) times ) ], "GPGPU23", Color = System.Drawing.Color.Black)
     ]

do System.Windows.Forms.Application.Run(gch2.ShowChart())

let getArgs3 i fn =
    let observSpace = RF00038.observSpace
    let stateSpace = RF00038.stateSpace
    let startProbs = RF00038.startProbs
    let transitionProbs = RF00038.transitionProbs
    let emissionProbs = RF00038.emissionProbs
    let observSeq = getArLn i
    fn [|0..observSpace.Length - 1|] stateSpace.Length startProbs [|for i in observSeq -> Array.findIndex ((=)i) observSpace|] transitionProbs emissionProbs

(getArgs3 10 Viterbi.Cons.viterbi) |> ignore
(getArgs3 10 Viterbi.Parallel.viterbi) |> ignore
(getArgs3 10 Viterbi.GPGPU.viterbi) |> ignore

let gch3 =     
    Chart.Combine
     [
      Chart.Line( [ for i in 20..5..140 -> (i, algImplTime (fun () -> (getArgs3 i Viterbi.Cons.viterbi) |> ignore) times ) ], "Cons38", Color = System.Drawing.Color.Red)
      Chart.Line( [ for i in 20..5..140 -> (i, algImplTime (fun () -> (getArgs3 i Viterbi.Parallel.viterbi) |> ignore) times ) ], "CPU38", Color = System.Drawing.Color.Green)
      Chart.Line( [ for i in 20..5..140 -> (i, algImplTime (fun () -> (getArgs3 i Viterbi.GPGPU.viterbi) |> ignore) times ) ], "GPGPU38", Color = System.Drawing.Color.Black)
     ]

do System.Windows.Forms.Application.Run(gch3.ShowChart())
