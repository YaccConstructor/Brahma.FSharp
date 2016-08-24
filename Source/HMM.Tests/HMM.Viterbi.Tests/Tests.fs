module HMM.Viterbi.Tests.Src

open NUnit.Framework
open HMM.Viterbi.Tests.RF01315
open HMM.Viterbi.Tests.RF02468
open HMM.Viterbi.Tests.RF00038

let ``first`` x =
    let observSpace = [|"normal"; "cold"; "dizzy"|]
    let stateSpace = [|"Healthy"; "Fever"|]
    let startProbs = [|0.6; 0.4|]
    let transitionProbs = [| [|0.7; 0.3|]; [|0.4; 0.6|] |]
    let emissionProbs = [| [|0.5; 0.4; 0.1|]; [|0.1; 0.3; 0.6|] |]
    let observSeq = [|"normal"; "cold"; "dizzy"|]
    let stateSeq = [|"Healthy"; "Healthy"; "Fever"|]
    let res = x <| [|0..observSpace.Length - 1|]
                <| stateSpace.Length
                <| startProbs 
                <| [|for i in observSeq -> Array.findIndex ((=)i) observSpace|] 
                <| transitionProbs 
                <| emissionProbs
    Assert.AreEqual(stateSeq, [|for i in res -> stateSpace.[i]|])

let ``hmmTestRF02468`` x =
    let observSpace = RF02468.observSpace
    let stateSpace = RF02468.stateSpace
    let startProbs = RF02468.startProbs
    let transitionProbs = RF02468.transitionProbs
    let emissionProbs = RF02468.emissionProbs
    let observeSeq = [|"G"; "A"; "G"; "C"; "G"; "G"; "C"; "G"; "G"; "C"; "A"; "A"; "C"; "U"; "A"; "C"; "C"; "C"; "U"; "G"; "C"; "G"; "G"; "C"; "C"; "A"; "C"; "G"; "G"; "C"; "U"; "G"; "C"; "U"; "G"; "C"; "C"; "U"; "end"|]
    let stateSeq = [|"M1"; "M2"; "M3"; "M4"; "M5"; "M6"; "M7"; "M8"; "M9"; "M10"; "M11"; "M12"; "M13"; "M14"; "M15"; "M16"; "M17"; "M18"; "M19"; "M20"; "M21"; "M22"; "M23"; "M24"; "M25"; "M26"; "M27"; "M28"; "M29"; "M30"; "M31"; "M32"; "M33"; "M34"; "M35"; "M36"; "M37"; "M38"; "E"|]
    let res = x <| [|0..observSpace.Length - 1|]
                <| stateSpace.Length
                <| startProbs 
                <| [|for i in observeSeq -> Array.findIndex ((=)i) observSpace|] 
                <| transitionProbs 
                <| emissionProbs
    Assert.AreEqual(stateSeq, [|for i in res -> stateSpace.[i]|])

let ``hmmTestRF01315`` x =
    let observSpace = RF01315.observSpace
    let stateSpace = RF01315.stateSpace
    let startProbs = RF01315.startProbs
    let transitionProbs = RF01315.transitionProbs
    let emissionProbs = RF01315.emissionProbs
    let observSeq = [|"U"; "U"; "U"; "C"; "A"; "A"; "U"; "U"; "C"; "C"; "U"; "U"; "A"; "U"; "A"; "G"; "G"; "U"; "A"; "A"; "G"; "C"; "U"; "A"; "A"; "C"; "G"; "A"; "C"; "C"; "end"|]
    let stateSeq = [|"M2"; "M3"; "M4"; "M5"; "M6"; "M7"; "M8"; "M9"; "M10"; "M11"; "M12"; "M13"; "M14"; "M15"; "M16"; "M17"; "M18"; "M19"; "M20"; "M21"; "M22"; "M23"; "M24"; "M25"; "M26"; "M27"; "M28"; "M29"; "M30"; "I30"; "E"|]
    let res = x <| [|0..observSpace.Length - 1|]
                <| stateSpace.Length
                <| startProbs 
                <| [|for i in observSeq -> Array.findIndex ((=)i) observSpace|] 
                <| transitionProbs 
                <| emissionProbs
    Assert.AreEqual(stateSeq, [|for i in res -> stateSpace.[i]|])

let ``hmmTestRF00038`` x =
    let observSpace = RF00038.observSpace
    let stateSpace = RF00038.stateSpace
    let startProbs = RF00038.startProbs
    let transitionProbs = RF00038.transitionProbs
    let emissionProbs = RF00038.emissionProbs
    let observSeq = [|"U"; "G"; "U"; "A"; "A"; "A"; "A"; "A"; "A"; "C"; "A"; "U"; "C"; "A"; "U"; "U"; "U"; "A"; "G"; "C"; "G"; "U"; "G"; "A"; "C"; "U"; "U"; "U"; "C"; "U"; "U"; "U"; "C"; "A"; "A"; "C"; "A"; "G"; "C"; "U"; "A"; "A"; "C"; "A"; "A"; "U"; "U"; "G"; "U"; "U"; "G"; "U"; "U"; "A"; "C"; "U"; "G"; "C"; "C"; "U"; "A"; "A"; "U"; "G"; "U"; "A"; "A"; "U"; "U"; "U"; "U"; "U"; "A"; "G"; "G"; "G"; "U"; "A"; "A"; "U"; "U"; "U"; "U"; "A"; "A"; "A"; "A"; "A"; "A"; "G"; "G"; "G"; "C"; "G"; "A"; "U"; "A"; "A"; "A"; "A"; "A"; "A"; "C"; "G"; "A"; "U"; "U"; "G"; "G"; "G"; "G"; "G"; "A"; "U"; "G"; "A"; "C"; "G"; "A"; "C"; "A"; "U"; "G"; "A"; "A"; "C"; "G"; "C"; "U"; "C"; "A"; "A"; "G"; "C"; "A"; "end"|]
    let stateSeq = [|"M1"; "M2"; "M3"; "M4"; "M5"; "M6"; "M7"; "M8"; "M9"; "M10"; "M11"; "M12"; "M13"; "M14"; "M15"; "M16"; "M17"; "M18"; "M19"; "M20"; "M21"; "M22"; "M23"; "M24"; "M25"; "M26"; "M27"; "M28"; "M29"; "M30"; "M31"; "M32"; "M33"; "M34"; "M35"; "M36"; "M37"; "M38"; "M39"; "M40"; "M41"; "M42"; "M43"; "M44"; "M45"; "M46"; "M47"; "M48"; "M49"; "M50"; "M51"; "M52"; "M53"; "M54"; "M55"; "M56"; "M57"; "M58"; "M59"; "M60"; "M61"; "M62"; "M63"; "M64"; "M65"; "I65"; "I65"; "M66"; "M67"; "M68"; "M69"; "M70"; "M71"; "M72"; "M73"; "M74"; "M75"; "M76"; "M77"; "M78"; "M79"; "M80"; "M81"; "M82"; "M83"; "M84"; "M85"; "M86"; "M87"; "M88"; "M89"; "M90"; "M91"; "M92"; "M93"; "M94"; "M95"; "M96"; "M97"; "M98"; "M99"; "M100"; "M101"; "M102"; "M103"; "M104"; "M105"; "M106"; "M107"; "M108"; "M109"; "M110"; "M111"; "M112"; "M113"; "M114"; "I114"; "M115"; "M116"; "M117"; "M118"; "M119"; "M120"; "M121"; "M122"; "M123"; "M124"; "M125"; "M126"; "M127"; "M128"; "M129"; "M130"; "M131"; "M132"; "E"|]
    let res = x <| [|0..observSpace.Length - 1|]
                <| stateSpace.Length
                <| startProbs 
                <| [|for i in observSeq -> Array.findIndex ((=)i) observSpace|]
                <| transitionProbs 
                <| emissionProbs
    Assert.AreEqual(stateSeq, [|for i in res -> stateSpace.[i]|])

let ``hmmTestRF01123`` x=
    let observSpace = RF01123.observSpace
    let stateSpace = RF01123.stateSpace
    let startProbs = RF01123.startProbs
    let transitionProbs = RF01123.transitionProbs
    let emissionProbs = RF01123.emissionProbs
    let observSeq = [|"A"; "A"; "A"; "A"; "A"; "U"; "G"; "A"; "U"; "G"; "A"; "G"; "U"; "C"; "A"; "C"; "G"; "C"; "G"; "G"; "G"; "C"; "C"; "A"; "C"; "C"; "U"; "G"; "A"; "G"; "C"; "G"; "G"; "U"; "G"; "A"; "U"; "C"; "C"; "C"; "A"; "A"; "G"; "U"; "C"; "U"; "G"; "A"; "U"; "U"; "G"; "C"; "end"|]
    let stateSeq = [|"M1"; "M2"; "M3"; "M4"; "M5"; "M6"; "M7"; "M8"; "M9"; "M10"; "M11"; "M12"; "M13"; "I13"; "M14"; "M15"; "M16"; "I16"; "M17"; "M18"; "M19"; "M20"; "M21"; "M22"; "M23"; "M24"; "M25"; "M26"; "M27"; "M28"; "M29"; "M30"; "M31"; "M32"; "M33"; "M34"; "M35"; "M45"; "M46"; "M47"; "M48"; "M49"; "M50"; "M51"; "M52"; "M53"; "M54"; "M55"; "M56"; "M57"; "I57"; "I57"; "E"|]
    let res = x <| [|0..observSpace.Length - 1|]
                <| stateSpace.Length
                <| startProbs 
                <| [|for i in observSeq -> Array.findIndex ((=)i) observSpace|] 
                <| transitionProbs 
                <| emissionProbs
    Assert.AreEqual(stateSeq, [|for i in res -> stateSpace.[i]|]) 
