module AtomicTests

open Expecto
open FSharp.Quotations.Evaluator
open FSharp.Quotations
open Brahma.FSharp.Tests.CustomDatatypes
open Expecto.Logging
open Expecto.Logging.Message
open FsCheck
open Brahma.FSharp

[<AutoOpen>]
module Helpers =
    let logger = Log.create "AtomicTests"

    type NormalizedFloatArray =
        static member FloatType() =
            Arb.Default.NormalFloat()
            |> Arb.toGen
            |> Gen.map float
            |> Gen.filter
                (fun v ->
                    v < 100. &&
                    v > -100.
                )
            |> Gen.arrayOf
            |> Arb.fromGen

        static member Float32Type() =
            Arb.Default.Float32()
            |> Arb.toGen
            |> Gen.filter
                (fun v ->
                    v < 100.f &&
                    v > -100.f
                )
            |> Gen.arrayOf
            |> Arb.fromGen

    module Settings =
        let wgSize = 256
        let doubledWgSize = wgSize * 2

        let fscheckConfig =
            { Config.QuickThrowOnFailure with
                QuietOnSuccess = true
                MaxTest = 20
                Arbitrary = [typeof<NormalizedFloatArray>]
            }
    let checkDefault<'a when 'a : equality and 'a : struct> context expected kernel =
        let actual =
            opencl {
                use! result = ClArray.toDevice <| Array.zeroCreate<'a> 1
                do! runCommand kernel <| fun kernelPrepare ->
                    kernelPrepare
                    <| Range1D(Settings.doubledWgSize, Settings.wgSize)
                    <| result

                return! ClArray.toHost result
            }
            |> ClTask.runSync context
            |> fun result -> result.[0]

        "Results should be equal"
        |> Expect.equal actual expected

// TODO add tests in inc dec on supported types (generate spinlock)

/// Stress test for unary atomic operations.
/// Use global atomics
let stressTest<'a when 'a : equality and 'a : struct> context (f: Expr<'a -> 'a>) size rawF (isEqual: 'a -> 'a -> bool) =
    let kernel =
        <@
            fun (range: Range1D) (result: 'a clarray) ->
                let gid = range.GlobalID0
                if gid < size then
                    atomic %f result.[0] |> ignore
        @>

    let expected =
        [0 .. size - 1]
        |> List.fold (fun state _ -> rawF state) Unchecked.defaultof<'a>

    let actual =
        opencl {
            use! result = ClArray.toDevice <| Array.zeroCreate<'a> 1
            do! runCommand kernel <| fun kernelPrepare ->
                kernelPrepare
                <| Range1D.CreateValid(size, Settings.wgSize)
                <| result

            return! ClArray.toHost result
        }
        |> ClTask.runSync context
        |> fun result -> result.[0]

    logger.debug (
        eventX "\nActual: {act}.\nExpected: {exp}"
        >> setField "act" actual
        >> setField "exp" expected
    )

    "Results should be equal"
    |> Expect.isTrue (isEqual actual expected)

let stressTestCases context = [
    let range = [1 .. 10 .. 100]

    // int
    yield! range |> List.map (fun size ->
        testCase $"Smoke stress test (size %i{size}) on atomic 'inc' on int" <| fun () ->
            stressTest<int> context <@ inc @> size (fun x -> x + 1) (=)
    )
    yield! range |> List.map (fun size ->
        testCase $"Smoke stress test (size %i{size}) on atomic 'dec' on int" <| fun () ->
            stressTest<int> context <@ dec @> size (fun x -> x - 1) (=)
    )

    // float32
    yield! range |> List.map (fun size ->
        testCase $"Smoke stress test (size %i{size}) on atomic 'inc' on float32" <| fun () ->
            stressTest<float32> context <@ fun x -> x + 1.f @> size (fun x -> x + 1.f) (fun x y -> float (abs (x - y)) < Accuracy.low.relative)
    )

    // double
    yield! range |> List.map (fun size ->
        testCase $"Smoke stress test (size %i{size}) on atomic 'inc' on float" <| fun () ->
            stressTest<float> context <@ fun x -> x + 1. @> size (fun x -> x + 1.) (fun x y -> abs (x - y) < Accuracy.low.relative)
    )

    // bool
    yield! range |> List.map (fun size ->
        testCase $"Smoke stress test (size %i{size}) on atomic 'not' on bool" <| fun () ->
            stressTest<bool> context <@ not @> size not (=)
    )

    // WrappedInt (не работает транляция или типа того)
    let wrappedIntInc = <@ fun x -> x + WrappedInt(1) @>
    yield! range |> List.map (fun size ->
        ptestCase $"Smoke stress test (size %i{size}) on custom atomic 'inc' on WrappedInt" <| fun () ->
            stressTest<WrappedInt> context wrappedIntInc size (fun x -> x + WrappedInt(1)) (=)
    )

    // custom int op
    let incx2 = <@ fun x -> x + 2 @>
    yield! range |> List.map (fun size ->
        testCase $"Smoke stress test (size %i{size}) on atomic unary func on int" <| fun () ->
            stressTest<int> context incx2 size (fun x -> x + 2) (=)
    )
]

/// Test for add and sub like atomic operations.
/// Use local and global atomics,
/// use reading from global mem in local atomic
let foldTest<'a when 'a : equality and 'a : struct> context f (isEqual: 'a -> 'a -> bool) =
    let (.=.) left right = isEqual left right |@ $"%A{left} = %A{right}"

    Check.One(Settings.fscheckConfig, fun (array: 'a[]) ->
        let arrayLength = array.Length
        let kernel zero =
            <@
                fun (range: Range1D) (array: 'a clarray) (result: 'a clcell) ->
                    let lid = range.LocalID0
                    let gid = range.GlobalID0

                    let localResult = localArray<'a> 1
                    if lid = 0 then
                        localResult.[0] <- zero

                    barrierLocal ()

                    if gid < arrayLength then
                        atomic %f localResult.[0] array.[gid] |> ignore

                    if lid = 0 then
                        atomic %f result.Value localResult.[0] |> ignore
            @>

        let expected () =
            array
            |> Array.fold (fun state x -> f.Evaluate() state x) Unchecked.defaultof<'a>

        let actual () =
            opencl {
                use! result = ClCell.alloc<'a> ()
                use! array = ClArray.toDevice array
                do! runCommand (kernel Unchecked.defaultof<'a>) <| fun kernelPrepare ->
                    kernelPrepare
                    <| Range1D.CreateValid(array.Length, Settings.wgSize)
                    <| array
                    <| result

                return! ClCell.toHost result
            }
            |> ClTask.runSync context

        array.Length <> 0
        ==> lazy (actual () .=. expected ())
    )

let foldTestCases context = [
    // int, smoke tests
    testCase "Smoke fold test atomic 'add' on int" <| fun () -> foldTest<int> context <@ (+) @> (=)

    // float
    testCase "Fold test atomic 'add' on float32" <| fun () -> foldTest<float32> context <@ (+) @> (fun x y -> float (abs (x - y)) < Accuracy.low.relative)

    // double
    testCase "Fold test atomic 'add' on float" <| fun () -> foldTest<float> context <@ (+) @> (fun x y -> abs (x - y) < Accuracy.low.relative)

    // bool
    ptestCase "Fold test atomic '&&' on bool" <| fun () -> foldTest<bool> context <@ (&&) @> (=)

    testCase "Reduce test atomic 'min' on int" <| fun () -> foldTest<int> context <@ min @> (=)
    ptestCase "Reduce test atomic 'min' on int64" <| fun () -> foldTest<int64> context <@ min @> (=)
    testCase "Reduce test atomic 'min' on int16" <| fun () -> foldTest<int16> context <@ min @> (=)

    testCase "Reduce test atomic 'max' on int" <| fun () -> foldTest<int> context <@ max @> (=)
    ptestCase "Reduce test atomic 'max' on int64" <| fun () -> foldTest<int64> context <@ max @> (=)
    testCase "Reduce test atomic 'max' on int16" <| fun () -> foldTest<int16> context <@ max @> (=)

    testCase "Reduce test atomic '&&&' on int" <| fun () -> foldTest<int> context <@ (&&&) @> (=)
    ptestCase "Reduce test atomic '&&&' on int64" <| fun () -> foldTest<int64> context <@ (&&&) @> (=)

    testCase "Reduce test atomic '|||' on int" <| fun () -> foldTest<int> context <@ (|||) @> (=)
    ptestCase "Reduce test atomic '|||' on int64" <| fun () -> foldTest<int64> context <@ (|||) @> (=)

    testCase "Reduce test atomic '^^^' on int" <| fun () -> foldTest<int> context <@ (^^^) @> (=)
    ptestCase "Reduce test atomic '^^^' on int64" <| fun () -> foldTest<int64> context <@ (^^^) @> (=)

    // WrappedInt (не работает транляция или типа того)
    ptestCase "Fold test atomic 'add' on WrappedInt" <| fun () -> foldTest<WrappedInt> context <@ (+) @> (=)
]

let xchgTest<'a when 'a : equality and 'a : struct> context cmp value =
    let localSize = Settings.wgSize
    let kernel =
        <@
            fun (range: Range1D) (array: 'a clarray) ->
                let gid = range.GlobalID0

                let localBuffer = localArray<'a> localSize
                atomic xchg localBuffer.[gid] array.[gid] |> ignore
                atomic cmpxchg array.[gid] cmp localBuffer.[localSize - gid] |> ignore
        @>

    let expected = Array.create<'a> localSize value

    let actual =
        opencl {
            use! buffer = ClArray.toDevice [| for i = 0 to localSize - 1 do if i < localSize / 2 then cmp else value |]
            do! runCommand kernel <| fun kernelPrepare ->
                kernelPrepare
                <| Range1D(localSize, localSize)
                <| buffer

            return! ClArray.toHost buffer
        }
        |> ClTask.runSync context

    "Results should be equal"
    |> Expect.sequenceEqual actual expected

let xchgTestCases context = [
    testCase "Xchg test on int" <| fun () -> xchgTest<int> context 0 256
    testCase "Xchg test on float" <| fun () -> xchgTest<float> context 0. 256.
    testCase "Xchg test on bool" <| fun () -> xchgTest<bool> context false true
    ptestCase "Xchg test on WrappedInt" <| fun () -> xchgTest<WrappedInt> context (WrappedInt 0) (WrappedInt 256)
]

// TODO barrier broken
let perfomanceTest context = fun () ->
    // use native atomic_inc for int
    let kernelUsingNativeInc =
        <@
            fun (range: Range1D) (result: int clarray) ->
                let localAcc = localArray<int> 1
                if range.LocalID0 = 0 then
                    localAcc.[0] <- 0

                atomic inc localAcc.[0] |> ignore
                barrierLocal ()

                if range.LocalID0 = 0 then
                    result.[0] <- localAcc.[0]
        @>

    // generate spinlock
    let kernelUsingCustomInc =
        let inc = <@ fun x -> x + 1 @>
        <@
            fun (range: Range1D) (result: int clarray) ->
                let localAcc = localArray<int> 1
                if range.LocalID0 = 0 then
                    localAcc.[0] <- 0

                atomic %inc localAcc.[0] |> ignore
                barrierLocal ()

                if range.LocalID0 = 0 then
                    result.[0] <- localAcc.[0]
        @>

    let prepare kernel () =
        opencl {
            use! result = ClArray.toDevice <| Array.zeroCreate<int> 1
            do! runCommand kernel <| fun kernelPrepare ->
                kernelPrepare
                <| Range1D(Settings.wgSize, Settings.wgSize)
                <| result

            return! ClArray.toHost result
        }
        |> ClTask.runSync context

    "Kernel wich uses native 'inc' should be faster than with custom one"
    |> Expect.isFasterThan (prepare kernelUsingNativeInc) (prepare kernelUsingCustomInc)

let tests context =
    [
        testList "Stress tests" << stressTestCases
        ptestList "Fold tests" << foldTestCases
        ptestList "Xchg tests" << xchgTestCases
        ptestCase "Perfomance test on 'inc'" << perfomanceTest
    ]
    |> List.map (fun testFixture -> testFixture context)

