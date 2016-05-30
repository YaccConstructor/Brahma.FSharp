module HMM.Viterbi.Tests.GPGPU

open Viterbi_Parallel_GPGPU
open NUnit.Framework
open HMM.Viterbi.Tests.Src

[<Test>]
let ``first`` () =
    HMM.Viterbi.Tests.Src.``first`` viterbi
