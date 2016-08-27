module Brahman.Test

open NUnit.Framework
open Brahman.Substrings.Helpers
open Brahman.Substrings.Matcher

[<TestFixture>]
type ``Brahman Substrings tests`` () = 
    let matcher = new Brahman.Substrings.Matcher.Matcher(1UL * 1024UL * 1024UL)

    [<Test>]
    member this.``RabinKarp simple test 1`` () = 
        let template = [|1uy;1uy|]
        let zSeq  = seq {for i in 0 .. 10000 do if i = 0 then yield! template else yield 0uy}        
        let res = matcher.RabinKarp (zSeq,[|template|])
        Assert.AreEqual(res.Data.Length, 1)
        Assert.AreEqual(res.Data.[0].ChunkNum,0)
        Assert.AreEqual(res.Data.[0].Offset,0)
        Assert.AreEqual(res.Data.[0].PatternId,0)

    [<Test>]
    member this.``RabinKarp simple test 2`` () = 
        let template = [|1uy;1uy|]
        let zSeq  = seq {for i in 0 .. 10000 do if i = 10 then yield! template else yield 0uy}        
        let res = matcher.RabinKarp (zSeq,[|template|])
        Assert.AreEqual(res.Data.Length,1)
        Assert.AreEqual(res.Data.[0].ChunkNum,0)
        Assert.AreEqual(res.Data.[0].Offset,10)
        Assert.AreEqual(res.Data.[0].PatternId,0)

    [<Test>]
    member this.``RabinKarp two patterns 1.`` () = 
        let templates = [|[|1uy;1uy|];[|2uy;2uy|]|]
        let zSeq  = seq {for i in 0 .. 10000 do if i = 10 then yield! templates.[1] else yield 0uy}        
        let res = matcher.RabinKarp (zSeq,templates)
        Assert.AreEqual(res.Data.Length,1)
        Assert.AreEqual(res.Data.[0].ChunkNum,0)
        Assert.AreEqual(res.Data.[0].Offset,10)
        Assert.AreEqual(res.Data.[0].PatternId, Array.findIndex ((=)templates.[1]) res.Templates )

    [<Test>]
    member this.``RabinKarp two patterns 2.`` () = 
        let templates = [|[|1uy;1uy|];[|2uy;2uy|]|]
        let zSeq  = seq {for i in 0 .. 10000 do if i = 10 then yield! templates.[1] elif i = 19 then yield! templates.[0] else yield 0uy}        
        let res = matcher.RabinKarp (zSeq,templates)
        Assert.AreEqual(res.Data.Length,2)
        Assert.AreEqual(res.Data.[0].ChunkNum,0)
        Assert.AreEqual(res.Data.[0].Offset,10)
        Assert.AreEqual(res.Data.[1].Offset,20)
        Assert.AreEqual(res.Data.[0].PatternId, Array.findIndex ((=)templates.[1]) res.Templates)
        Assert.AreEqual(res.Data.[1].PatternId, Array.findIndex ((=)templates.[0]) res.Templates)


    [<Test>]
    member this.``RabinKarp two chanks 1`` () = 
        let template = [|1uy;1uy|]
        let x = 1024 * 1024 / 3 + 5000
        let zSeq  = seq {for i in 0 .. 1024 * 1024 do if i = x  then yield! template else yield 0uy}        
        let res = matcher.RabinKarp (zSeq,[|template|])
        Assert.AreEqual(res.Data.Length,1)
        Assert.AreEqual(res.Data.[0].ChunkNum,1)
        Assert.AreEqual(x - res.ChunkSize + 32, res.Data.[0].Offset)
        Assert.AreEqual(res.Data.[0].PatternId,0)

//[<EntryPoint>]
//let f x =
//    let t = new ``Brahman Substrings tests``()
//    //t.``RabinKarp simple test 2``()
//    //t.``RabinKarp simple test 2``()
//    //t.``RabinKarp two patterns 2.``()
//    t.``RabinKarp two chanks 1``()
//    1