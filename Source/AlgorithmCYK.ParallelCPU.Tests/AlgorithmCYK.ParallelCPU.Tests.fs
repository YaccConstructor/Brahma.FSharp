module AlgorithmCYK.ParallelCPU.Tests
open AlgorithmCYK
open AlgorithmCYK.ParallelCPU
open NUnit.Framework

[<TestFixture>]

type TestsGrammar1() = 
    
    let (rl1, non1, start1) = ([("S","AB"); ("S","BC"); ("A","BA"); ("B","CC"); ("C","AB"); ("B","b"); ("A","a"); ("C","a")], [|"S";"A";"B";"C"|], "S")

    [<Test>]
    member this.``CYKParallelCPU returns true`` () =
      let result = CYKParallelCPU rl1 non1 "baaba" start1
      Assert.AreEqual(true, result)

    [<Test>]
    member this.``CYKParallelCPU returns false`` () =
      let result = CYKParallelCPU rl1 non1 "baaababbbabab" start1
      Assert.AreEqual(false, result)

    [<Test>]
    member this.``CYKParallelCPU returns true (empty string)`` () =
      let result = CYKParallelCPU rl1 non1 "" start1
      Assert.AreEqual(true, result)

type TestsGrammar2() = 

    let (rl2, non2, start2) = ([("S","AB"); ("A","CC"); ("B","BC"); ("C","CB"); ("C","BA"); ("B","b"); ("A","a"); ("A","c"); ("C","c")], [|"S";"A";"B";"C"|],"S")

    [<Test>]
    member this.``CYKParallelCPU returns true`` () =
      let result = CYKParallelCPU rl2 non2 "bccbcabcb" start2
      Assert.AreEqual(true, result)

    [<Test>]
    member this.``CYKParallelCPU returns false`` () =
      let result = CYKParallelCPU rl2 non2 "cccccaaaabbb" start2
      Assert.AreEqual(false, result)

    [<Test>]
    member this.``CYKParallelCPU returns true (empty string)`` () =
      let result = CYKParallelCPU rl2 non2 "" start2
      Assert.AreEqual(true, result)