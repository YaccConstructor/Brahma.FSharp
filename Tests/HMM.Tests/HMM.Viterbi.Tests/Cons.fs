﻿module HMM.Viterbi.Tests.Cons

open Viterbi.Cons
open NUnit.Framework
open HMM.Viterbi.Tests.Src

[<Test>]
let small () =
    HMM.Viterbi.Tests.Src.small viterbi

[<Test>]
let RF01315q62 () =
    HMM.Viterbi.Tests.Src.hmmTestRF01315 viterbi

[<Test>]
let RF02468q78 () =
    HMM.Viterbi.Tests.Src.hmmTestRF02468 viterbi

[<Test>]
let RF01123q116 () =
    HMM.Viterbi.Tests.Src.hmmTestRF01123 viterbi

[<Test>]
let ``RF00038q266l`` () =
    HMM.Viterbi.Tests.Src.hmmTestRF00038 viterbi

[<Test>]
let ``only end`` () =
    HMM.Viterbi.Tests.Src.hmmTestRF02468_1 viterbi