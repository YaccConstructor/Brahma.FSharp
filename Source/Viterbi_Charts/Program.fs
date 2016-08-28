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

let getArgs i fn  =
    let observSpace = RF02468.observSpace (*RF02468 RF01123 RF00038*)
    let stateSpace = RF02468.stateSpace (*RF02468 RF01123 RF00038*)
    let startProbs = RF02468.startProbs (*RF02468 RF01123 RF00038*)
    let transitionProbs = RF02468.transitionProbs (*RF02468 RF01123 RF00038*)
    let emissionProbs = RF02468.emissionProbs (*RF02468 RF01123 RF00038*)
    let observSeq = getArLn i
    fn [|0..observSpace.Length - 1|] stateSpace.Length startProbs [|for i in observSeq -> Array.findIndex ((=)i) observSpace|] transitionProbs emissionProbs

(getArgs 10 Viterbi.Cons.viterbi) |> ignore
(getArgs 10 Viterbi.Parallel.viterbi) |> ignore
(getArgs 10 Viterbi.GPGPU.viterbi) |> ignore

let gch =     
    Chart.Combine
     [
      Chart.Line( [ for i in 10..10..180 -> (i, algImplTime (fun () -> (getArgs i Viterbi.Cons.viterbi) |> ignore) times ) ], "Cons68", Color = System.Drawing.Color.Red)
      Chart.Line( [ for i in 10..10..180 -> (i, algImplTime (fun () -> (getArgs i Viterbi.Parallel.viterbi) |> ignore) times )  ], "CPU68", Color = System.Drawing.Color.Green)
      Chart.Line( [ for i in 10..10..180 -> (i, algImplTime (fun () -> (getArgs i Viterbi.GPGPU.viterbi) |> ignore) times ) ], "GPGPU68", Color = System.Drawing.Color.Black)
     ]

do System.Windows.Forms.Application.Run(gch.ShowChart()) 
