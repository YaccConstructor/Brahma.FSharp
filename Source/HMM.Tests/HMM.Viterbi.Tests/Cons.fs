module HMM.Viterbi.Tests.Cons

open Viterbi_Cons
open NUnit.Framework
open HMM.Viterbi.Tests.Src

[<Test>]
let ``first`` () =
    HMM.Viterbi.Tests.Src.``first`` viterbi

[<Test>]
let ``RF01315`` () =
    HMM.Viterbi.Tests.Src.hmmTestRF01315 viterbi

[<Test>]
let ``RF02468`` () =
    HMM.Viterbi.Tests.Src.hmmTestRF02468 viterbi
