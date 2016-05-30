module HMM.Viterbi.Tests.Cons

open Viterbi_Cons
open NUnit.Framework
open HMM.Viterbi.Tests.Src

[<Test>]
let ``first`` () =
    HMM.Viterbi.Tests.Src.``first`` viterbi
