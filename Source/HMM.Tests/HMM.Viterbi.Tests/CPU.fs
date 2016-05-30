module HMM.Viterbi.Tests.CPU

open Viterbi_Parallel_CPU
open NUnit.Framework
open HMM.Viterbi.Tests.Src

[<Test>]
let ``first`` () =
    HMM.Viterbi.Tests.Src.``first`` viterbi

