module Viterbi_Cons.Tests

open Viterbi_Cons
open NUnit.Framework


[<Test>]
let ``first`` () =
   let observSpace = [|"normal"; "cold"; "dizzy"|]
   let stateSpace = [|"Healthy"; "Fever"|]
   let startProbs = [|0.6; 0.4|]
   let transitionProbs = [| [|0.7; 0.3|]; [|0.4; 0.6|] |]
   let emissionProbs = [| [|0.5; 0.4; 0.1|]; [|0.1; 0.3; 0.6|] |]
   let observeSeq = [|"normal"; "cold"; "dizzy"|]
   let stateSeq = [|"Healthy"; "Healthy"; "Fever"|]
   let res = viterbi 
             <| [|0..observSpace.Length - 1|]
             <| [|0..stateSpace.Length - 1|] 
             <| startProbs 
             <| [|for i in observeSeq -> Array.findIndex (fun x -> x = i) observSpace|] 
             <| transitionProbs 
             <| emissionProbs
   Assert.AreEqual(stateSeq, [|for i in res -> stateSpace.[i]|])