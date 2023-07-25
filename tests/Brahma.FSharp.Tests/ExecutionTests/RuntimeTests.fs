module RuntimeTests

open Expecto
open FSharp.Quotations
open Expecto.Logging
open Brahma.FSharp

[<AutoOpen>]
module Helpers =
    let defaultInArrayLength = 4
    let intInArr = [| 0 .. defaultInArrayLength - 1 |]
    let float32Arr = Array.init defaultInArrayLength float32
    let default1D = Range1D(defaultInArrayLength, 1)
    let default2D = Range2D(defaultInArrayLength, 1)

    let checkResult context command (inArr: 'a[]) (expectedArr: 'a[]) =
        let actual =
            opencl {
                use! inBuf = ClArray.toDevice inArr
                do! runCommand command <| fun x ->
                    x default1D inBuf

                return! ClArray.toHost inBuf
            }
            |> ClTask.runSync context

        Expect.sequenceEqual actual expectedArr $"For context: %A{context}. Arrays should be equals"

let logger = Log.create "FullTests"

let smokeTestsOnPrimitiveTypes context = [
    let inline checkResult cmd input expected = checkResult context cmd input expected

    testCase "Array item set" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf: ClArray<int>) ->
                    buf.[0] <- 1
            @>

        checkResult command intInArr [|1; 1; 2; 3|]

    testCase "Array item set. Long" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf: ClArray<_>) ->
                    buf.[0] <- 1L
            @>

        checkResult command [|0L; 1L; 2L; 3L|] [|1L; 1L; 2L; 3L|]

    testCase "Array item set. ULong" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf: ClArray<uint64>) ->
                    buf.[0] <- 1UL
            @>

        checkResult command [|0UL; 1UL; 2UL; 3UL|] [|1UL; 1UL; 2UL; 3UL|]

    testCase "Array item set. Sbyte" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf: ClArray<sbyte>) ->
                    buf.[0] <- 1y
            @>

        checkResult command [|0y; 1y; 2y; 3y|] [|1y; 1y; 2y; 3y|]

    testCase "Array item set. Sequential operations" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf: ClArray<int>) ->
                    buf.[0] <- 2
                    buf.[1] <- 4
            @>

        checkResult command intInArr [|2; 4; 2; 3|]

    testCase "Byte type support with overflow" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf: ClArray<byte>) ->
                    if range.GlobalID0 = 0 then
                        buf.[0] <- buf.[0] + 1uy
                        buf.[1] <- buf.[1] + 1uy
                        buf.[2] <- buf.[2] + 1uy
            @>

        checkResult command [|0uy; 255uy; 254uy|] [|1uy; 0uy; 255uy|]
]

let typeCastingTests context = [
    let inline checkResult cmd input expected = checkResult context cmd input expected

    testCase "uint64 -> int64" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf: ClArray<int64>) ->
                    buf.[0] <- int64 1UL
            @>

        checkResult command [|0L; 1L|] [|1L; 1L|]

    testCase "int64 -> uint64" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf: ClArray<uint64>) ->
                    buf.[0] <- uint64 1L
            @>

        checkResult command [|0UL; 1UL|] [|1UL; 1UL|]

    testCase "byte -> float -> byte" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf: ClArray<byte>) ->
                    if range.GlobalID0 = 0 then
                        buf.[0] <- byte (float buf.[0])
                        buf.[1] <- byte (float buf.[1])
                        buf.[2] <- byte (float buf.[2])
            @>

        checkResult command [|0uy; 255uy; 254uy|] [|0uy; 255uy; 254uy|]

    // test fail on Intel platform:
    // Actual: [1uy, 255uy, 255uy]
    ptestCase "Byte and float 2" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf: ClArray<byte>) ->
                    if range.GlobalID0 = 0 then
                        buf.[0] <- byte ((float buf.[0]) + 1.0)
                        buf.[1] <- byte ((float buf.[1]) + 1.0)
                        buf.[2] <- byte ((float buf.[2]) + 1.0)
            @>

        checkResult command [|0uy; 255uy; 254uy|] [|1uy; 0uy; 255uy|]

    // test failed on Intel platform:
    // Actual : [1uy, 1uy, 1uy]
    ptestCase "Byte and float in condition" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf: ClArray<byte>) ->
                    if range.GlobalID0 = 0 then
                        let x = if true then buf.[0] + 1uy else buf.[0] + 1uy
                        buf.[0] <- x
                        let y = if true then buf.[1] + 1uy else buf.[1] + 1uy
                        buf.[1] <- y
                        let z = if true then buf.[2] + 1uy else buf.[2] + 1uy
                        buf.[2] <- z
            @>

        checkResult command [|0uy; 255uy; 254uy|] [|1uy; 0uy; 255uy|]

    // test failed on Intel platform due to exception
    ptestCase "Byte and float in condition 2" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf: ClArray<byte>) ->
                    if range.GlobalID0 = 0
                    then
                        let x =
                            if true
                            then
                                let g = 1uy
                                buf.[0] + g
                            else buf.[0] + 1uy
                        buf.[0] <- x
                        let y =
                            if true
                            then
                                let g = 1uy
                                buf.[1] + g
                            else buf.[1] + 1uy
                        buf.[1] <- y
                        let z =
                            if true
                            then
                                let g = 1uy
                                buf.[2] + g
                            else buf.[2] + 1uy
                        buf.[2] <- z
            @>

        checkResult command [|0uy; 255uy; 254uy|] [|1uy; 0uy; 255uy|]
]

let bindingTests context = [
    let inline checkResult cmd input expected = checkResult context cmd input expected

    testCase "Bindings. Simple" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf: ClArray<int>) ->
                    let x = 1
                    buf.[0] <- x
            @>

        checkResult command intInArr [|1; 1; 2; 3|]

    testCase "Bindings. Sequential bindings" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf: ClArray<int>) ->
                    let x = 1
                    let y = x + 1
                    buf.[0] <- y
            @>

        checkResult command intInArr [|2; 1; 2; 3|]

    testCase "Bindings. Binding in IF" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf: ClArray<int>) ->
                    if 2 = 0 then
                        let x = 1
                        buf.[0] <- x
                    else
                        let i = 2
                        buf.[0] <- i
            @>

        checkResult command intInArr [|2; 1; 2; 3|]

    testCase "Bindings. Binding in FOR" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf: ClArray<int>) ->
                    for i in 0..3 do
                        let x = i * i
                        buf.[i] <- x
            @>

        checkResult command intInArr [|0; 1; 4; 9|]

    testCase "Bindings. Binding in WHILE" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf: ClArray<int>) ->
                    while buf.[0] < 5 do
                        let x = buf.[0] + 1
                        buf.[0] <- x * x
            @>

        checkResult command intInArr [|25; 1; 2; 3|]
]

let operatorsAndMathFunctionsTests context =
    let inline checkResult cmd input expected = checkResult context cmd input expected

    let binaryOpTestGen testCase name
        (binop: Expr<'a -> 'a -> 'a>)
        (xs: array<'a>)
        (ys: array<'a>)
        (expected: array<'a>) =

        testCase name <| fun () ->
            let command =
                <@
                    fun (range:  Range1D) (xs: ClArray<'a>) (ys: ClArray<'a>) (zs: ClArray<'a>) ->
                        let i = range.GlobalID0
                        zs.[i] <- (%binop) xs.[i] ys.[i]
                @>

            let range = Range1D <| Array.length expected
            let zs = Array.zeroCreate <| Array.length expected

            let actual =
                opencl {
                    use! inBufXs = ClArray.toDevice xs
                    use! inBufYs = ClArray.toDevice ys
                    use! outBuf = ClArray.toDevice zs

                    do! runCommand command <| fun x ->
                        x range inBufXs inBufYs outBuf

                    return! ClArray.toHost outBuf
                }
                |> ClTask.runSync context

            Expect.sequenceEqual actual expected ":("

    let unaryOpTestGen testCase name
        (unop: Expr<'a -> 'a>)
        (xs: array<'a>)
        (expected: array<'a>) =

        testCase name <| fun () ->
            let command =
                <@
                    fun (range:  Range1D) (xs: ClArray<'a>) (zs: ClArray<'a>) ->
                        let i = range.GlobalID0
                        zs.[i] <- (%unop) xs.[i]
                @>

            let range = Range1D <| Array.length expected
            let zs = Array.zeroCreate <| Array.length expected

            let actual =
                opencl {
                    use! inBufXs = ClArray.toDevice xs
                    use! outBuf = ClArray.toDevice zs

                    do! runCommand command <| fun x ->
                        x range inBufXs outBuf

                    return! ClArray.toHost outBuf
                }
                |> ClTask.runSync context

            Expect.sequenceEqual actual expected ":("

    [
        binaryOpTestGen testCase "Boolean OR" <@ (||) @>
            [|true; false; false; true|]
            [|false; true; false; true|]
            [|true; true; false; true|]

        binaryOpTestGen testCase "Boolean AND" <@ (&&) @>
            [|true; false; false; true|]
            [|false; true; false; true|]
            [|false; false; false; true|]

        binaryOpTestGen testCase "Bitwise OR on int" <@ (|||) @>
            [|1; 0; 0; 1|]
            [|0; 1; 0; 1|]
            [|1; 1; 0; 1|]

        binaryOpTestGen testCase "Bitwise AND on int" <@ (&&&) @>
            [|1; 0; 0; 1|]
            [|0; 1; 0; 1|]
            [|0; 0; 0; 1|]

        binaryOpTestGen testCase "Bitwise XOR on int" <@ (^^^) @>
            [|1; 0; 0; 1|]
            [|0; 1; 0; 1|]
            [|1; 1; 0; 0|]

        binaryOpTestGen testCase "Arithmetic PLUS on int" <@ (+) @>
            [|1; 2; 3; 4|]
            [|5; 6; 7; 8|]
            [|6; 8; 10; 12|]

        unaryOpTestGen testCase "Bitwise NEGATION on int" <@ (~~~) @>
            <|| (
                [|1; 10; 99; 0|]
                |> fun array -> array, array |> Array.map (fun x -> - x - 1)
            )

        binaryOpTestGen testCase "MAX on float32" <@ max @>
            [|1.f; 2.f; 3.f; 4.f|]
            [|5.f; 6.f; 7.f; 8.f|]
            [|5.f; 6.f; 7.f; 8.f|]

        binaryOpTestGen testCase "MIN on float32" <@ min @>
            [|1.f; 2.f; 3.f; 4.f|]
            [|5.f; 6.f; 7.f; 8.f|]
            [|1.f; 2.f; 3.f; 4.f|]

        ptestCase "MAX on int16 with const" <| fun () ->
            let command =
                <@
                    fun (range: Range1D) (buf: int16 clarray) ->
                        let gid = range.GlobalID0
                        buf.[gid] <- max buf.[gid] 1s
                @>

            let inA = [|0s; 1s; 2s; 3s|]
            checkResult command inA (Array.map (max 1s) inA)

        // Failed: due to precision
        ptestCase "Math sin" <| fun _ ->
            let command =
                <@
                    fun (range: Range1D) (buf: ClArray<float>) ->
                        let i = range.GlobalID0
                        buf.[i] <- System.Math.Sin (float buf.[i])
                @>

            let inA = [|0.0; 1.0; 2.0; 3.0|]
            checkResult command inA (inA |> Array.map System.Math.Sin)  //[|0.0; 0.841471; 0.9092974; 0.14112|]
    ]

let controlFlowTests context = [
    let inline checkResult cmd input expected = checkResult context cmd input expected

    testCase "Check 'if then' condition" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf: ClArray<int>) ->
                    if 0 = 2 then buf.[0] <- 42
            @>

        checkResult command intInArr [|0; 1; 2; 3|]

    testCase "Check 'if then else' condition" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf: ClArray<int>) ->
                    if 0 = 2 then buf.[0] <- 1 else buf.[0] <- 2
            @>

        checkResult command intInArr [|2; 1; 2; 3|]

    testCase "Check 'for' integer loop" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf: ClArray<int>) ->
                    if range.GlobalID0 = 0 then
                        for i in 0 .. 3 do
                            buf.[i] <- i
            @>

        checkResult command intInArr [|0; 1; 2; 3|]

    testCase "Check 'for' integer loop with step" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf: ClArray<int>) ->
                    if range.GlobalID0 = 0 then
                        for i in 0 .. 2 .. 6 do
                            buf.[i / 2] <- i
            @>

        checkResult command intInArr [|0; 2; 4; 6|]

    testCase "Check 'for' non-integer loop" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf: ClArray<uint32>) ->
                    if range.GlobalID0 = 0 then
                        for i in 0u .. 3u do
                            buf.[int i] <- i
            @>

        checkResult command [|0u; 0u; 0u; 0u|] [|0u; 1u; 2u; 3u|]

    testCase "Check simple 'while' loop" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf: ClArray<int>) ->
                    while buf.[0] < 5 do
                        buf.[0] <- buf.[0] + 1
            @>

        checkResult command intInArr [|5; 1; 2; 3|]

    testCase "Check 'while' loop inside 'for' integer loop" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf: ClArray<int>) ->
                    for i in 0 .. 3 do
                        while buf.[i] < 10 do
                            buf.[i] <- buf.[i] * buf.[i] + 1
            @>

        checkResult command intInArr [|26; 26; 26; 10|]
]

let kernelArgumentsTests context = [
    let inline checkResult cmd input expected = checkResult context cmd input expected

    testCase "Simple 1D" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf: ClArray<int>) ->
                    let i = range.GlobalID0
                    buf.[i] <- i + i
            @>

        checkResult command intInArr [|0;2;4;6|]

    testCase "Simple 1D with copy" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (inBuf:ClArray<int>) (outBuf:ClArray<int>) ->
                    let i = range.GlobalID0
                    outBuf.[i] <- inBuf.[i]
            @>

        let expected = [|0; 1; 2; 3|]

        let actual =
            opencl {
                use! inBuf = ClArray.toDevice intInArr
                use! outBuf = ClArray.toDevice [|0; 0; 0; 0|]
                do! runCommand command <| fun x ->
                    x default1D inBuf outBuf

                return! ClArray.toHost inBuf
            }
            |> ClTask.runSync context

        Expect.sequenceEqual actual expected  "Arrays should be equals"

    testCase "Simple 1D float" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf: ClArray<float32>) ->
                    let i = range.GlobalID0
                    buf.[i] <- buf.[i] * buf.[i]
            @>

        checkResult command float32Arr [|0.0f; 1.0f; 4.0f; 9.0f|]

    testCase "Int as arg" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) x (buf: ClArray<int>) ->
                    let i = range.GlobalID0
                    buf.[i] <- x + x
            @>

        let expected = [|4; 4; 4; 4|]

        let actual =
            opencl {
                use! inBuf = ClArray.toDevice intInArr
                do! runCommand command <| fun x ->
                    x default1D 2 inBuf

                return! ClArray.toHost inBuf
            }
            |> ClTask.runSync context

        Expect.sequenceEqual actual expected "Arrays should be equals"

    testCase "Sequential commands over single buffer" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) i x (buf: ClArray<int>) ->
                    buf.[i] <- x + x
            @>

        let expected = [|4; 1; 4; 3|]

        let actual =
            opencl {
                use! inArr = ClArray.toDevice intInArr

                do! runCommand command <| fun it ->
                    it
                    <| default1D
                    <| 0
                    <| 2
                    <| inArr

                do! runCommand command <| fun it ->
                    it
                    <| default1D
                    <| 2
                    <| 2
                    <| inArr

                return! ClArray.toHost inArr
            }
            |> ClTask.runSync context

        Expect.sequenceEqual actual expected "Arrays should be equals"

    ptestProperty "Parallel execution of kernel" <| fun _const ->
        let context = context.ClContext
        let n = 4
        let l = 256
        let getAllocator (context: ClContext) =
            let kernel =
                <@
                    fun (r: Range1D) (buffer: ClArray<int>) ->
                        let i = r.GlobalID0
                        buffer.[i] <- _const
                @>
            let k = context.Compile kernel
            fun (q:MailboxProcessor<_>) ->
                let buf = context.CreateClArray(l, allocationMode = AllocationMode.AllocHostPtr)
                let executable = k.GetKernel()
                q.Post(Msg.MsgSetArguments(fun () -> executable.KernelFunc (Range1D(l, l)) buf))
                q.Post(Msg.CreateRunMsg<_,_>(executable))
                buf

        let allocator = getAllocator context
        let allocOnGPU (q:MailboxProcessor<_>) allocator =
            let b = allocator q
            let res = Array.zeroCreate l
            q.PostAndReply (fun ch -> Msg.CreateToHostMsg(b, res, ch)) |> ignore
            q.Post (Msg.CreateFreeMsg b)
            res

        let actual =
            Array.init n (fun _ -> context.QueueProvider.CreateQueue())
            |> Array.map (fun q -> async { return allocOnGPU q allocator })
            |> Async.Parallel
            |> Async.RunSynchronously

        let expected = Array.init n (fun _ -> Array.create l _const)

        Expect.sequenceEqual actual expected "Arrays should be equals"
]

let quotationInjectionTests context = [
    let inline checkResult cmd input expected = checkResult context cmd input expected

    testCase "Quotations injections 1" <| fun _ ->
        let myF = <@ fun x -> x * x @>

        let command =
            <@
                fun (range: Range1D) (buf: ClArray<int>) ->
                    buf.[0] <- (%myF) 2
                    buf.[1] <- (%myF) 4
            @>

        checkResult command intInArr [|4;16;2;3|]

    testCase "Quotations injections 2" <| fun _ ->
        let myF = <@ fun x y -> y - x @>

        let command =
            <@
                fun (range: Range1D) (buf: ClArray<int>) ->
                    buf.[0] <- (%myF) 2 5
                    buf.[1] <- (%myF) 4 9
            @>

        checkResult command intInArr [|3;5;2;3|]
]

let localMemTests context = [
    let inline checkResult cmd input expected = checkResult context cmd input expected

    // TODO: pointers to local data must be local too.
    testCase "Local int. Work item counting" <| fun _ ->
        let command =
            <@
                fun (range:  Range1D) (output: ClArray<int>) ->
                    let globalID = range.GlobalID0
                    let mutable x = local ()

                    if globalID = 0 then x <- 0
                    barrierLocal ()

                    atomic (+) x 1 |> ignore
                    // fetch local value before read, dont work without barrier
                    barrierLocal ()

                    if globalID = 0 then
                        output.[0] <- x
            @>

        let expected = [|5|]

        let actual =
            opencl {
                use! inBuf = ClArray.toDevice [|0|]
                do! runCommand command <| fun x ->
                    x (Range1D(5, 5)) inBuf

                return! ClArray.toHost inBuf
            }
            |> ClTask.runSync context

        Expect.sequenceEqual actual expected "Arrays should be equals"

    testCase "Local array. Test 1" <| fun _ ->
        let localWorkSize = 5
        let globalWorkSize = 15

        let command =
            <@
                fun (range: Range1D) (input: ClArray<int>) (output: ClArray<int>) ->
                    let localBuf = localArray<int> localWorkSize

                    localBuf.[range.LocalID0] <- range.LocalID0
                    barrierLocal ()
                    output.[range.GlobalID0] <- localBuf.[(range.LocalID0 + 1) % localWorkSize]
            @>


        let expected =
            [| for x in 1..localWorkSize -> x % localWorkSize |]
            |> Array.replicate (globalWorkSize / localWorkSize)
            |> Array.concat

        let actual =
            opencl {
                use! inBuf = ClArray.toDevice (Array.zeroCreate globalWorkSize)
                use! outBuf = ClArray.toDevice (Array.zeroCreate globalWorkSize)
                do! runCommand command <| fun x ->
                    x (Range1D(globalWorkSize, localWorkSize)) inBuf outBuf

                return! ClArray.toHost outBuf
            }
            |> ClTask.runSync context

        Expect.sequenceEqual actual expected "Arrays should be equals"

    ptestCase "Local array. Test 2" <| fun _ ->
        let command =
            <@ fun (range:  Range1D) (buf:  ClArray<int64>) ->
                let localBuf = localArray 42
                atomic xchg localBuf.[0] 1L |> ignore
                buf.[0] <- localBuf.[0]
            @>

        checkResult command [|0L; 1L; 2L; 3L|] [|1L; 1L; 2L; 3L|]
]

let letTransformationTests context = [
    let inline checkResult cmd input expected = checkResult context cmd input expected

    testCase "Template Let Transformation Test 0" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf:  ClArray<int>) ->
                    let f = 3
                    buf.[0] <- f
            @>

        checkResult command intInArr [|3; 1; 2; 3|]

    testCase "Template Let Transformation Test 1" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf:  ClArray<int>) ->
                    let x = 4
                    let f =
                        let x = 3
                        x
                    buf.[0] <- x + f
            @>
        checkResult command intInArr [|7; 1; 2; 3|]

    testCase "Template Let Transformation Test 1.2" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf:  ClArray<int>) ->
                    let f y =
                        let x c b = b + c + 4 + y
                        x 2 3
                    buf.[0] <- f 1
            @>

        checkResult command intInArr [|10; 1; 2; 3|]

    testCase "Template Let Transformation Test 2" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf:  ClArray<int>) ->
                    let f =
                        let x =
                            let y = 3
                            y
                        x
                    buf.[0] <- f
            @>

        checkResult command intInArr [|3; 1; 2; 3|]

    testCase "Template Let Transformation Test 3" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf:  ClArray<int>) ->
                    let f =
                        let f = 5
                        f
                    buf.[0] <- f
            @>

        checkResult command intInArr [|5; 1; 2; 3|]

    testCase "Template Let Transformation Test 4" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf:  ClArray<int>) ->
                    let f =
                        let f =
                            let f = 5
                            f
                        f
                    buf.[0] <- f
            @>

        checkResult command intInArr [|5; 1; 2; 3|]

    testCase "Template Let Transformation Test 5" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf:  ClArray<int>) ->
                    let f a b =
                        let x y z = y + z
                        x a b
                    buf.[0] <- f 1 7
            @>

        checkResult command intInArr [|8; 1; 2; 3|]

    testCase "Template Let Transformation Test 6" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf:  ClArray<int>) ->
                    let f x y =
                        let x = x
                        x + y
                    buf.[0] <- f 7 8
            @>

        checkResult command intInArr [|15; 1; 2; 3|]

    testCase "Template Let Transformation Test 7" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf:  ClArray<int>) ->
                    let f y =
                        let x y = 6 - y
                        x y
                    buf.[0] <- f 7
            @>

        checkResult command intInArr [|-1; 1; 2; 3|]

    testCase "Template Let Transformation Test 8" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (m: ClArray<int>) ->
                    let p = m.[0]
                    let x n =
                        let l = m.[3]
                        let g k = k + m.[0] + m.[1]
                        let r =
                            let y a =
                                let x = 5 - n + (g 4)
                                let z t = m.[2] + a - t
                                z (a + x + l)
                            y 6
                        r + m.[3]
                    if range.GlobalID0 = 0
                    then m.[0] <- x 7
            @>

        checkResult command intInArr [|-1; 1; 2; 3|]

    testCase "Template Let Transformation Test 9" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf:  ClArray<int>) ->
                    let x n =
                        let r = 8
                        let h = r + n
                        h
                    buf.[0] <- x 9
            @>

        checkResult command intInArr [|17; 1; 2; 3|]

    testCase "Template Let Transformation Test 10" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf:  ClArray<int>) ->
                    let p = 9
                    let x n b =
                        let t = 0
                        n + b + t
                    buf.[0] <- x 7 9
            @>

        checkResult command intInArr [|16; 1; 2; 3|]

    testCase "Template Let Transformation Test 11" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf:  ClArray<int>) ->
                    let p = 1
                    let m =
                        let r (l:int) = l
                        r 9
                    let z (k:int) = k
                    buf.[0] <- m
            @>

        checkResult command intInArr [|9; 1; 2; 3|]

    testCase "Template Let Transformation Test 12" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf:  ClArray<int>) ->
                    let f x y =
                        let y = y
                        let y = y
                        let g x m = m + x
                        g x y
                    buf.[0] <- f 1 7
            @>

        checkResult command intInArr [|8; 1; 2; 3|]

    testCase "Template Let Transformation Test 13" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf:  ClArray<int>) ->
                    let f y =
                        let y = y
                        let y = y
                        let g (m:int) = m
                        g y
                    buf.[0] <- f 7
            @>

        checkResult command intInArr [|7; 1; 2; 3|]

    testCase "Template Let Transformation Test 14" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf:  ClArray<int>) ->
                    let f y =
                        let y = y
                        let y = y
                        let g (m:int) =
                            let g r t = r + y - t
                            let n o = o - (g y 2)
                            n 5
                        g y
                    let z y = y - 2
                    buf.[0] <- f (z 7)
            @>

        checkResult command intInArr [|-3; 1; 2; 3|]

    testCase "Template Let Transformation Test 15" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf:  ClArray<int>) ->
                    let f y =
                        let Argi index =
                            if index = 0
                            then buf.[1]
                            else buf.[2]
                        Argi y
                    buf.[0] <- f 0
            @>

        checkResult command intInArr [|1; 1; 2; 3|]

    testCase "Template Let Transformation Test 16" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf:  ClArray<int>) ->
                    let f y =
                        if y = 0
                        then
                            let z (a:int) = a
                            z 9
                        else buf.[2]
                    buf.[0] <- f 0
            @>

        checkResult command intInArr [|9; 1; 2; 3|]

    testCase "Template Let Transformation Test 17" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf:  ClArray<int>) ->
                    if range.GlobalID0 = 0
                    then
                        let f y =
                            let g = buf.[1] + 1
                            y + g
                        for i in 0..3 do
                            buf.[i] <- f i
            @>

        checkResult command intInArr [|2; 3; 6; 7|]

    testCase "Template Let Transformation Test 18" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf:  ClArray<int>) ->
                    for i in 0..3 do
                        let f =
                            let g = buf.[1] + 1
                            i + g
                        if range.GlobalID0 = 0
                        then buf.[i] <- f
            @>

        checkResult command intInArr [|2; 3; 6; 7|]

    testCase "Template Let Transformation Test 19" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf:  ClArray<int>) ->
                    if range.GlobalID0 = 0
                    then
                        for i in 0..3 do
                            let f x =
                                let g = buf.[1] + x
                                i + g
                            buf.[i] <- f 1
            @>

        checkResult command intInArr [|2; 3; 6; 7|]

    // TODO: perform range (1D, 2D, 3D) erasure when range is lifted.
    ptestCase "Template Let Transformation Test 20" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (m: ClArray<int>) ->
                    let f x =
                        range.GlobalID0 + x
                    m.[0] <- f 2
            @>

        checkResult command intInArr [|2; 3; 6; 7|]
]

let letQuotationTransformerSystemTests context = [
    let inline checkResult cmd input expected = checkResult context cmd input expected

    testCase "Test 0" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf:  ClArray<int>) ->
                    let mutable x = 1
                    let f y =
                        x <- y
                    f 10
                    buf.[0] <- x
            @>

        checkResult command intInArr [|10; 1; 2; 3|]

    testCase "Test 1" <| fun _ ->
        let command =
            <@
                fun (range:  Range1D) (buf:  ClArray<int>) ->
                    let mutable x = 1
                    let f y =
                        x <- x + y
                    f 10
                    buf.[0] <- x
            @>

        checkResult command intInArr [|11; 1; 2; 3|]

    testCase "Test 2" <| fun _ ->
        let command =
            <@
                fun (range:  Range1D) (arr: ClArray<int>) ->
                    let f x =
                        let g y = y + 1
                        g x
                    arr.[0] <- f 2
            @>

        checkResult command intInArr [|3; 1; 2; 3|]

    testCase "Test 3" <| fun _ ->
        let command =
            <@
                fun (range:  Range1D) (arr: ClArray<int>)->
                    let f x =
                        let g y =
                            y + x
                        g (x + 1)
                    arr.[0] <- f 2
            @>

        checkResult command intInArr [|5; 1; 2; 3|]

    testCase "Test 4" <| fun _ ->
        let command =
            <@
                fun (range:  Range1D) (arr: ClArray<int>) ->
                    let gid = range.GlobalID0
                    let x =
                        let mutable y = 0

                        let addToY x =
                            y <- y + x

                        for i in 0..5 do
                            addToY arr.[gid]
                        y
                    arr.[gid] <- x
            @>

        checkResult command intInArr [|0; 6; 12; 18|]

    testCase "Test 5" <| fun _ ->
        let command =
            <@
                fun (range:  Range1D) (arr: ClArray<int>) ->
                    let gid = range.GlobalID0

                    let mutable x =
                        if 0 > 1 then 2 else 3

                    let mutable y =
                        for i in 0..4 do
                            x <- x + 1
                        x + 1

                    let z =
                        x + y

                    let f () =
                        arr.[gid] <- x + y + z
                    f ()
            @>

        checkResult command intInArr [|34; 34; 34; 34|]
]

let commonApiTests context = [
    let inline checkResult cmd input expected = checkResult context cmd input expected

    testCase "Check simple '|> ignore'" <| fun () ->
        let command =
            <@
                fun (range:  Range1D) (buffer: ClArray<int>) ->
                    let gid = range.GlobalID0
                    atomic inc buffer.[gid] |> ignore
            @>

        checkResult command intInArr (intInArr |> Array.map ((+) 1))

    // Lambda is not supported.
    ptestCase "Forward pipe" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf: ClArray<int>) ->
                    buf.[0] <- (1.25f |> int)
            @>
        checkResult command intInArr [|1; 1; 2; 3|]

    // Lambda is not supported.
    ptestCase "Backward pipe" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf: ClArray<int>) ->
                    buf.[0] <- int <| 1.25f + 2.34f
            @>
        checkResult command intInArr [|3; 1; 2; 3|]

    testCase "Getting value of 'int clcell' should be correct" <| fun () ->
        let command =
            <@
                fun (range: Range1D) (buffer: int clarray) (cell: int clcell) ->
                    let gid = range.GlobalID0
                    buffer.[gid] <- cell.Value
            @>

        let value = 10
        let expected = Array.replicate defaultInArrayLength value

        let actual =
            opencl {
                use! cell = ClCell.toDevice 10
                use! buffer = ClArray.alloc<int> defaultInArrayLength
                do! runCommand command <| fun it ->
                    it
                    <| default1D
                    <| buffer
                    <| cell

                return! ClArray.toHost buffer
            }
            |> ClTask.runSync context

        "Arrays should be equal"
        |> Expect.sequenceEqual actual expected

    // TODO test on getting Value property of non-clcell type
    // TODO test on getting Item property on non-clarray type

    testCase "Setting value of 'int clcell' should be correct" <| fun () ->
        let value = 10
        let command =
            <@
                fun (range: Range1D) (cell: int clcell) ->
                    cell.Value <- value
            @>

        let actual =
            opencl {
                use! cell = ClCell.toDevice value
                do! runCommand command <| fun it ->
                    it
                    <| default1D
                    <| cell

                return! ClCell.toHost cell
            }
            |> ClTask.runSync context

        "Arrays should be equal"
        |> Expect.equal actual value

    testCase "Using 'int clcell' from inner function should work correctly" <| fun () ->
        let value = 10
        let command =
            <@
                fun (range: Range1D) (cell: int clcell) ->
                    let f () =
                        let x = cell.Value
                        cell.Value <- x

                    f ()
            @>

        let actual =
            opencl {
                use! cell = ClCell.toDevice value
                do! runCommand command <| fun it ->
                    it
                    <| default1D
                    <| cell

                return! ClCell.toHost cell
            }
            |> ClTask.runSync context

        "Arrays should be equal"
        |> Expect.equal actual value

    testCase "Using 'int clcell' with native atomic operation should be correct" <| fun () ->
        let value = 10
        let command =
            <@
                fun (range: Range1D) (cell: int clcell) ->
                    atomic (+) cell.Value value |> ignore
            @>

        let expected = value * default1D.GlobalWorkSize

        let actual =
            opencl {
                use! cell = ClCell.toDevice 0
                do! runCommand command <| fun it ->
                    it
                    <| default1D
                    <| cell

                return! ClCell.toHost cell
            }
            |> ClTask.runSync context

        "Arrays should be equal"
        |> Expect.equal actual expected

    ptestCase "Using 'int clcell' with spinlock atomic operation should be correct" <| fun () ->
        let value = 10
        let command =
            <@
                fun (range: Range1D) (cell: int clcell) ->
                    atomic (fun x -> x + value) cell.Value |> ignore
            @>

        let expected = value * default1D.GlobalWorkSize

        let actual =
            opencl {
                use! cell = ClCell.toDevice 0
                do! runCommand command <| fun it ->
                    it
                    <| default1D
                    <| cell

                return! ClCell.toHost cell
            }
            |> ClTask.runSync context

        "Arrays should be equal"
        |> Expect.equal actual expected
]

let booleanTests context = [
    testCase "Executing copy kernel on boolean array should not raise exception" <| fun () ->
        let inputArray = Array.create 100_000 true
        let inputArrayLength = inputArray.Length
        let copy =
            <@
                fun (ndRange: Range1D)
                    (inputArrayBuffer: bool clarray)
                    (outputArrayBuffer: bool clarray) ->

                    let i = ndRange.GlobalID0
                    if i < inputArrayLength then
                        outputArrayBuffer.[i] <- inputArrayBuffer.[i]
            @>

        let actual =
            opencl {
                use! input = ClArray.toDevice inputArray
                use! output = ClArray.alloc<bool> 100_000
                do! runCommand copy <| fun x ->
                    x
                    <| Range1D.CreateValid(inputArray.Length, 256)
                    <| input
                    <| output

                return! ClArray.toHost output
            }
            |> ClTask.runSync context

        "Arrays should be equal"
        |> Expect.sequenceEqual actual inputArray

    testProperty "'lor' on boolean type should work correctly" <| fun (array: bool[]) ->
        if array.Length <> 0 then
            let reversed = Seq.rev array |> Seq.toArray
            let inputArrayLength = array.Length
            let command =
                <@
                    fun (ndRange: Range1D)
                        (array: bool clarray)
                        (reversed: bool clarray) ->

                        let i = ndRange.GlobalID0
                        if i < inputArrayLength then
                            array.[i] <- array.[i] || reversed.[i] || false
                @>

            let expected =
                (array, reversed)
                ||> Array.zip
                |> Array.map (fun (x, y) -> x || y)

            let actual =
                opencl {
                    use! array' = ClArray.toDevice array
                    use! reversed' = ClArray.toDevice reversed
                    do! runCommand command <| fun x ->
                        x
                        <| Range1D.CreateValid(inputArrayLength, 256)
                        <| array'
                        <| reversed'

                    return! ClArray.toHost array'
                }
                |> ClTask.runSync context

            "Arrays should be equal"
            |> Expect.sequenceEqual actual expected

    testProperty "'land' on boolean type should work correctly" <| fun (array: bool[]) ->
        if array.Length <> 0 then
            let reversed = Seq.rev array |> Seq.toArray
            let inputArrayLength = array.Length
            let command =
                <@
                    fun (ndRange: Range1D)
                        (array: bool clarray)
                        (reversed: bool clarray) ->

                        let i = ndRange.GlobalID0
                        if i < inputArrayLength then
                            array.[i] <- array.[i] && reversed.[i] && true
                @>

            let expected =
                (array, reversed)
                ||> Array.zip
                |> Array.map (fun (x, y) -> x && y)

            let actual =
                opencl {
                    use! array' = ClArray.toDevice array
                    use! reversed' = ClArray.toDevice reversed
                    do! runCommand command <| fun x ->
                        x
                        <| Range1D.CreateValid(inputArrayLength, 256)
                        <| array'
                        <| reversed'

                    return! ClArray.toHost array'
                }
                |> ClTask.runSync context

            "Arrays should be equal"
            |> Expect.sequenceEqual actual expected
]

let parallelExecutionTests context = [
    testCase "Running tasks in parallel should not raise exception" <| fun () ->
        let fill = opencl {
            let kernel =
                <@
                    fun (range: Range1D) (buffer: int clarray) ->
                        let i = range.GlobalID0
                        buffer.[i] <- 1
                @>

            use! array = ClArray.alloc<int> 256
            do! runCommand kernel <| fun x ->
                x
                <| Range1D.CreateValid(256, 256)
                <| array

            return! ClArray.toHost array
        }

        let expected = Array.replicate 3 (Array.create 256 1)

        let actual =
            opencl {
                return!
                    List.replicate 3 fill
                    |> ClTask.inParallel
            }
            |> ClTask.runSync context

        "Arrays should be equal"
        |> Expect.sequenceEqual actual expected

    // TODO check if it really faster
]

type Option1 =
    | None1
    | Some1 of int

let simpleDUTests context = [
    testCase "Option<int> with F#-native syntax" <| fun () ->
        let rnd = System.Random()
        let input1 = Array.init 100_000 (fun i -> rnd.Next())
        let input2 = Array.init 100_000 (fun i -> rnd.Next())
        let inputArrayLength = input1.Length
        let add (op:Expr<Option<int> -> Option<int> -> Option<int>>) =
            <@
                fun (ndRange: Range1D)
                    (input1: int clarray)
                    (input2: int clarray)
                    (output: int clarray) ->

                    let i = ndRange.GlobalID0
                    if i < inputArrayLength then
                        let x = if input1.[i] < 0 then None else Some input1.[i]
                        let y = if input2.[i] < 0 then None else Some input2.[i]
                        output.[i] <- match (%op) x y with Some x -> x | None -> 0
            @>

        let actual =
            opencl {
                use! input1 = ClArray.toDevice input1
                use! input2 = ClArray.toDevice input2
                use! output = ClArray.alloc<int> 100_000
                let op =
                    <@ fun x y ->
                        match x with
                        | Some x -> match y with Some y -> Some (x + y) | None -> Some x
                        | None -> match y with Some y -> Some y | None -> None
                    @>

                do! runCommand (add op) <| fun x ->
                    x
                    <| Range1D.CreateValid(input1.Length, 256)
                    <| input1
                    <| input2
                    <| output

                return! ClArray.toHost output
            }
            |> ClTask.runSync context

        let expected =
            (input1, input2)
            ||> Array.map2
                (fun x y ->
                    if x < 0 then
                        if y < 0 then 0 else y
                    else
                        x + y
                )

        "Arrays should be equal"
        |> Expect.sequenceEqual actual expected

    testCase "Option<int> with simplified syntax" <| fun () ->
        let rnd = System.Random()
        let input1 = Array.init 100_000 (fun i -> rnd.Next())
        let input2 = Array.init 100_000 (fun i -> rnd.Next())
        let inputArrayLength = input1.Length
        let add (op:Expr<Option<int> -> Option<int> -> Option<int>>) =
            <@
                fun (ndRange: Range1D)
                    (input1: int clarray)
                    (input2: int clarray)
                    (output: int clarray) ->

                    let i = ndRange.GlobalID0
                    if i < inputArrayLength then
                        let mutable x = None
                        let mutable y = None
                        if input1.[i] >= 0 then x <- Some input1.[i]
                        if input2.[i] >= 0 then y <- Some input2.[i]
                        match (%op) x y with
                        | Some x -> output.[i] <- x
                        | None -> output.[i] <- 0
            @>

        let actual =
            opencl {
                use! input1 = ClArray.toDevice input1
                use! input2 = ClArray.toDevice input2
                use! output = ClArray.alloc<int> 100_000
                let op =
                    <@ fun x y ->
                        match x, y with
                        | Some x, Some y -> Some (x + y)
                        | Some x, None -> Some x
                        | None, Some y -> Some y
                        | None, None -> None
                    @>

                do! runCommand (add op) <| fun x ->
                    x
                    <| Range1D.CreateValid(input1.Length, 256)
                    <| input1
                    <| input2
                    <| output

                return! ClArray.toHost output
            }
            |> ClTask.runSync context

        let expected =
            (input1, input2)
            ||> Array.map2
                (fun x y ->
                    if x < 0 then
                        if y < 0 then 0 else y
                    else
                        x + y
                )

        "Arrays should be equal"
        |> Expect.sequenceEqual actual expected

    testCase "Simple custom non-generic DU" <| fun () ->
        let rnd = System.Random()
        let input1 = Array.init 100_000 (fun i -> rnd.Next())
        let input2 = Array.init 100_000 (fun i -> rnd.Next())
        let inputArrayLength = input1.Length
        let add (op:Expr<Option1 -> Option1 -> Option1>) =
            <@
                fun (ndRange: Range1D)
                    (input1: int clarray)
                    (input2: int clarray)
                    (output: int clarray) ->

                    let i = ndRange.GlobalID0
                    if i < inputArrayLength then
                        let mutable x = None1
                        let mutable y = None1
                        if input1.[i] >= 0 then x <- Some1 input1.[i]
                        if input2.[i] >= 0 then y <- Some1 input2.[i]
                        let z = (%op) x y
                        match z with
                        | Some1 x -> output.[i] <- x
                        | None1 -> output.[i] <- 0
            @>

        let actual =
            opencl {
                use! input1 = ClArray.toDevice input1
                use! input2 = ClArray.toDevice input2
                use! output = ClArray.alloc<int> 100_000
                let op =
                    <@ fun x y ->
                        match x with
                        | Some1 x -> match y with Some1 y -> Some1 (x + y) | None1 -> Some1 x
                        | None1 -> match y with Some1 y -> Some1 y | None1 -> None1
                    @>

                do! runCommand (add op) <| fun x ->
                    x
                    <| Range1D.CreateValid(input1.Length, 256)
                    <| input1
                    <| input2
                    <| output

                return! ClArray.toHost output
            }
            |> ClTask.runSync context

        let expected =
            (input1, input2)
            ||> Array.map2
                (fun x y ->
                    if x < 0 then
                        if y < 0 then 0 else y
                    else
                        x + y
                )

        "Arrays should be equal"
        |> Expect.sequenceEqual actual expected
]

[<Struct>]
type StructWithOverridedConstructors =
    val mutable x: int
    val mutable y: int
    new(x, y) = { x = x; y = y }
    new(x) = { x = x; y = 10 }

//let specificTests = testList "" [
//    ptestCase "" <| fun () ->
//        let command =
//            <@
//                fun (range: Range1D) (buffer: ClCell<StructWithOverridedConstructors>) ->
//                    buffer.Value <- StructWithOverridedConstructors(10)
//            @>
//
//        let expected = StructWithOverridedConstructors(10, 10)
//
//        let actual =
//            opencl {
//                let value = 5
//                use! buffer = ClCell.toDevice <| StructWithOverridedConstructors(value, value)
//                do! runCommand command <| fun it ->
//                    it
//                    <| Range1D(1)
//                    <| buffer
//
//                return! ClCell.toHost buffer
//            }
//            |> ClTask.runSync context
//
//        ""
//        |> Expect.equal actual expected
//]

let tests context =
    [
        testList "Simple tests on primitive types" << smokeTestsOnPrimitiveTypes
        testList "Type castings tests" << typeCastingTests
        testList "Bindings tests" << bindingTests
        testList "Operators and math functions tests" << operatorsAndMathFunctionsTests
        testList "Control flow tests" << controlFlowTests
        testList "Kernel arguments tests" << kernelArgumentsTests
        testList "Quotation injection tests" << quotationInjectionTests
        testList "Local memory tests"  << localMemTests
        testList "Let Transformation Tests"  << letTransformationTests
        testList "Let Transformation Tests Mutable Vars" << letQuotationTransformerSystemTests
        testList "Common Api Tests" << commonApiTests
        testList "Boolean Tests" << booleanTests
        ptestList "Parallel Execution Tests" << parallelExecutionTests
        testList "Simple tests on discriminated unions" << simpleDUTests
    ]
    |> List.map (fun testFixture -> testFixture context)
