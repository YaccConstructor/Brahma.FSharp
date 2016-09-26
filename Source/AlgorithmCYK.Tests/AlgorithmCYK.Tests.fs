module CYK.Tests
open AlgorithmCYK
open NUnit.Framework

[<TestFixture>]

type TestsGrammar1() = 
    
    let (rl1, start1) = ([|("S","AB"); ("S","BC"); ("A","BA"); ("B","CC"); ("C","AB"); ("B","b"); ("A","a"); ("C","a")|], "S")

    [<Test>]
    member this.``CYK returns true`` () =
      let result = CYK rl1 "baaba" start1
      Assert.AreEqual(true, result)

    [<Test>]
    member this.``CYK returns false`` () =
      let result = CYK rl1 "baaababbbabab" start1
      Assert.AreEqual(false, result)

    [<Test>]
    member this.``CYK returns true (empty string)`` () =
      let result = CYK rl1 "" start1
      Assert.AreEqual(true, result)

type TestsGrammar2() = 

    let (rl2, start2) = ([|("S","AB"); ("A","CC"); ("B","BC"); ("C","CB"); ("C","BA"); ("B","b"); ("A","a"); ("A","c"); ("C","c")|], "S")

    [<Test>]
    member this.``CYK returns true`` () =
      let result = CYK rl2 "bccbcabcb" start2
      Assert.AreEqual(true, result)

    [<Test>]
    member this.``CYK returns false`` () =
      let result = CYK rl2 "cccccaaaabbb" start2
      Assert.AreEqual(false, result)

    [<Test>]
    member this.``CYK returns true (empty string)`` () =
      let result = CYK rl2 "" start2
      Assert.AreEqual(true, result)