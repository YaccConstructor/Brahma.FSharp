module WorkflowBuilderTests

open FSharp.Quotations
open Expecto
open System.Collections.Generic
open Brahma.FSharp

[<AutoOpen>]
module Helpers =
    let eqMsg = "Values should be equal"

    let gpuMap (f: Expr<'a -> 'b>) (input: 'a clarray) =
        opencl {
            let! res = ClArray.alloc<'b> input.Length

            let code =
                <@ fun (range: Range1D) (input: 'a clarray) (output: 'b clarray) ->
                    let idx = range.GlobalID0
                    output.[idx] <- (%f) input.[idx] @>

            do! runCommand code <| fun x ->
                x
                <| Range1D input.Length
                <| input
                <| res

            return res
        }

let bindTests context = [
    testCase "Test 1" <| fun _ ->
        let xs = [| 1; 2; 3; 4 |]

        let workflow =
            opencl {
                use! xs' = ClArray.toDevice xs
                use! ys = gpuMap <@ fun x -> x * x + 10 @> xs'
                use! zs = gpuMap <@ fun x -> x + 1 @> ys
                return! ClArray.toHost zs
            }

        let output = ClTask.runSync context workflow
        Expect.equal output [| 12; 15; 20; 27 |] eqMsg

    testCase "'use!' should free resources after all" <| fun () ->
        let log = ResizeArray()

        opencl {
            use! resource = opencl {
                return
                    { new System.IDisposable with
                        member this.Dispose() = log.Add "disposed"
                    }
            }

            do! opencl { return log.Add "1" }
            return! opencl { log.Add "2" }
        }
        |> ClTask.runSync context

        "Last value should be 'disposed'"
        |> Expect.isTrue (log.[log.Count - 1] = "disposed")
]

let loopTests context = [
    testCase "While. Test 1. Without evaluation" <| fun _ ->
        let mutable log : int list = []

        let workflow =
            opencl {
                let mutable i = 0
                log <- i :: log

                while i < 10 do
                    i <- i + 1
                    log <- i :: log
            }

        Expect.equal log [] "Delay should prevent any computations before evaluation started"
        ClTask.runSync context workflow
        Expect.equal log [ 10 .. -1 .. 0 ] eqMsg

    testCase "While. Test 2. Simple evaluation" <| fun _ ->
        let mutable xs = [| 1; 2; 3; 4; 5; 6; 7; 8 |]
        let iters = 5
        let expected = Array.map (fun x -> pown 2 iters * x) xs

        // TODO change to use copyTo
        let workflow =
            opencl {
                let f = <@ fun x -> x * 2 @>

                let mutable i = 0

                let! xs' = ClArray.toDevice xs
                let mutable tmp = xs'
                while i < iters do
                    let! res = gpuMap f tmp
                    do! ClArray.close tmp
                    tmp <- res
                    i <- i + 1

                let! res = ClArray.toHost tmp
                do! ClArray.close tmp

                return res
            }

        let output = ClTask.runSync context workflow
        Expect.equal output expected eqMsg

    testCase "While. Test 3. Do inside body of while loop" <| fun _ ->
        let gpuMapInplace f (xs: int clarray ref) =
            opencl {
                let! res = gpuMap f !xs
                do! ClArray.close !xs
                xs := res
            }

        let workflow =
            opencl {
                let! xs = ClArray.toDevice [| 1; 2; 3; 4 |]
                let xs = ref xs

                let mutable i = 0

                while i < 10 do
                    do! gpuMapInplace <@ fun x -> x + 1 @> xs
                    i <- i + 1

                return! ClArray.toHost !xs
            }

        let output = ClTask.runSync context workflow
        Expect.equal output [| 11; 12; 13; 14 |] eqMsg

    testCase "For. Test 1. Without evaluation" <| fun _ ->
        let log = List<int>()

        let workflow =
            opencl {
                log.Add(0)

                for x in [ 1 .. 10 ] do
                    log.Add(x)
            }

        Expect.sequenceEqual log
        <| List<int>()
        <| "Delay should prevent any computations before evaluation started"

        ClTask.runSync context workflow
        Expect.sequenceEqual log (List<int>([ 0 .. 10 ])) eqMsg

    testCase "For. Test 2. Simple evaluation" <| fun _ ->
        let workflow =
            opencl {
                let xs = [| 1; 2; 3; 4 |]
                let! xs' = ClArray.toDevice xs
                let mutable tmp = xs'

                for y in [| 10; 20; 30 |] do
                    let! res = gpuMap <@ fun x -> x + y @> tmp
                    do! ClArray.close tmp
                    tmp <- res

                return! ClArray.toHost tmp
            }

        let output = ClTask.runSync context workflow
        Expect.equal output [| 61; 62; 63; 64 |] eqMsg
]

let tests context =
    [
        testList "Simple bind tests" << bindTests
        testList "Loop tests" << loopTests
    ]
    |> List.map (fun testFixture -> testFixture context)
