module HMM.Viterbi.Tests.GPGPU

open Viterbi.GPGPU
open NUnit.Framework
open HMM.Viterbi.Tests.Src

[<Test>]
let ``first`` () =
    HMM.Viterbi.Tests.Src.``first`` viterbi

[<Test>]
let ``RF01315q62`` () =
    HMM.Viterbi.Tests.Src.hmmTestRF01315 viterbi

[<Test>]
let ``RF02468q78`` () =
    HMM.Viterbi.Tests.Src.hmmTestRF02468 viterbi

[<Test>]
let ``RF01123q116`` () =
    HMM.Viterbi.Tests.Src.hmmTestRF01123 viterbi

[<Test>]
let ``RF00038q266len135`` () =
    HMM.Viterbi.Tests.Src.hmmTestRF00038 viterbi

