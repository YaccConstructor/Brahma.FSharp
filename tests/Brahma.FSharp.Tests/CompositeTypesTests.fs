﻿module CompositeTypesTests

open Expecto
open Brahma.FSharp.OpenCL
open FSharp.Quotations
open Brahma.FSharp.Tests

// Incomplete pattern matching in record deconstruction
#nowarn "667"

[<Struct>]
type RecordOfIntInt64 =
    {
        X: int
        Y: int64
    }

[<Struct>]
type RecordOfBoolBool =
    {
        X: bool
        Y: bool
    }

[<Struct>]
type GenericRecord<'a, 'b> =
    {
        mutable X: 'a
        mutable Y: 'b
    }

[<Struct>]
type StructOfIntInt64 =
    val mutable X: int
    val mutable Y: int64
    new(x, y) = { X = x; Y = y }

[<Struct>]
type GenericStruct<'a, 'b> =
    val mutable X: 'a
    val mutable Y: 'b
    new(x, y) = { X = x; Y = y }

let check<'a when 'a : struct and 'a : equality> (data: 'a[]) (command: int -> Expr<Range1D -> ClArray<'a> -> unit>) =
    let length = data.Length

    let expected = data

    let actual =
        opencl {
            use! buffer = ClArray.toDevice data
            do! runCommand (command length) <| fun it ->
                it
                <| Range1D.CreateValid(data.Length, 256)
                <| buffer

            return! ClArray.toHost buffer
        }
        |> ClTask.runSync context

    "Arrays should be equal"
    |> Expect.sequenceEqual actual expected

let message typeName = sprintf "Simple test on `%s`" typeName

let tupleTestCases = testList "Tuple tests" [
    let inline command length =
        <@
            fun (gid: int) (buffer: clarray<struct('a * 'b)>) ->
                if gid < length then
                    let struct(a, b) = buffer.[gid]
                    buffer.[gid] <- struct(a, b)
        @>

    testProperty (message "struct(int * int)") <| fun (data: struct(int * int)[]) ->
        if data.Length <> 0 then check data (fun length -> <@ fun (range: Range1D) (buffer: ClArray<_>) -> (%command length) range.GlobalID0 buffer @>)

    testProperty (message "struct(int * int64)") <| fun (data: struct(int * int64)[]) ->
        if data.Length <> 0 then check data (fun length -> <@ fun (range: Range1D) (buffer: ClArray<_>) -> (%command length) range.GlobalID0 buffer @>)

    testProperty (message "struct(bool * bool") <| fun (data: struct(bool * bool)[]) ->
        if data.Length <> 0 then check data (fun length -> <@ fun (range: Range1D) (buffer: ClArray<_>) -> (%command length) range.GlobalID0 buffer @>)

    testProperty (message "struct((int * int) * (int * int))") <| fun (data: struct((int * int) * (int * int))[]) ->
        if data.Length <> 0 then check data (fun length -> <@ fun (range: Range1D) (buffer: ClArray<_>) -> (%command length) range.GlobalID0 buffer @>)

    testProperty (message "struct((int * int64) * (bool * bool))") <| fun (data: struct((int * int64) * (bool * bool))[]) ->
        if data.Length <> 0 then check data (fun length -> <@ fun (range: Range1D) (buffer: ClArray<_>) -> (%command length) range.GlobalID0 buffer @>)

    testProperty (message "struct(RecordOfIntInt64 * RecordOfBoolBool)") <| fun (data: struct(RecordOfIntInt64 * RecordOfBoolBool)[]) ->
        if data.Length <> 0 then check data (fun length -> <@ fun (range: Range1D) (buffer: ClArray<_>) -> (%command length) range.GlobalID0 buffer @>)

    testProperty (message "struct(GenericRecord<int, int64> * GenericRecord<bool, bool>)") <| fun (data: struct(GenericRecord<int, int64> * GenericRecord<bool, bool>)[]) ->
        if data.Length <> 0 then check data (fun length -> <@ fun (range: Range1D) (buffer: ClArray<_>) -> (%command length) range.GlobalID0 buffer @>)

    testProperty (message "struct(int * int64 * bool)") <| fun (data: struct(int * int64 * bool)[]) ->
        if data.Length <> 0 then
            check data <| fun length ->
                <@ fun (range: Range1D) (buffer: ClArray<_>) ->
                    let gid = range.GlobalID0
                    if gid < length then
                        let struct(a1, a2, a3) = buffer.[gid]
                        buffer.[gid] <- struct(a1, a2, a3)
                @>

    testProperty "Simple test on big tuple (of size 10)" <| fun (data: struct(int * int * int * int * int * int * int * int * int * int)[]) ->
        if data.Length <> 0 then
            check data <| fun length ->
                <@ fun (range: Range1D) (buffer: ClArray<_>) ->
                    let gid = range.GlobalID0
                    if gid < length then
                        let struct(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = buffer.[gid]
                        buffer.[gid] <- struct(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
                @>

    testProperty "Test on inner tuples deconstruction" <| fun (data: struct((int * int) * (int * int))[]) ->
        if data.Length <> 0 then
            check data <| fun length ->
                <@ fun (range: Range1D) (buffer: ClArray<_>) ->
                    let gid = range.GlobalID0
                    if gid < length then
                        let struct((a, b), (c, d)) = buffer.[gid]
                        buffer.[gid] <- struct((a, b), (c, d))
                @>
]

let recordTestCases = testList "Record tests" [
    let inline command length =
        <@
            fun (gid: int) (buffer: ClArray<GenericRecord<'a, 'b>>) ->
                if gid < length then
                    let { X = x; Y = y } = buffer.[gid]
                    let mutable innerStruct = { X = x; Y = y }
                    innerStruct.X <- x
                    innerStruct.Y <- y
                    buffer.[gid] <- { X = innerStruct.X; Y = innerStruct.Y }
        @>

    testProperty (message "GenericRecord<int, bool>") <| fun (data: GenericRecord<int, bool>[]) ->
        if data.Length <> 0 then check data (fun length -> <@ fun (range: Range1D) (buffer: ClArray<_>) -> (%command length) range.GlobalID0 buffer @>)

    testProperty (message "GenericRecord<(int * int64), (bool * bool)>") <| fun (data: GenericRecord<(int * int64), (bool * bool)>[]) ->
        if data.Length <> 0 then check data (fun length -> <@ fun (range: Range1D) (buffer: ClArray<_>) -> (%command length) range.GlobalID0 buffer @>)
]

let structTests = testList "Struct tests" [
    testCase "Smoke test" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf:  ClArray<StructOfIntInt64>) ->
                    if range.GlobalID0 = 0 then
                        let b = buf.[0]
                        buf.[0] <- buf.[1]
                        buf.[1] <- b
            @>

        checkResult command [|StructOfIntInt64(1, 2L); StructOfIntInt64(3, 4L)|]
                            [|StructOfIntInt64(3, 4L); StructOfIntInt64(1, 2L)|]

    testCase "Struct constructor test" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf:  ClArray<StructOfIntInt64>) ->
                    buf.[0] <- StructOfIntInt64(5, 6L)
            @>

        checkResult command [|StructOfIntInt64(1, 2L); StructOfIntInt64(3, 4L)|]
                            [|StructOfIntInt64(5, 6L); StructOfIntInt64(3, 4L)|]

    testCase "Struct prop set" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf:  ClArray<StructOfIntInt64>) ->
                    let mutable y = buf.[0]
                    y.X <- 5
                    buf.[0] <- y
            @>

        checkResult command [|StructOfIntInt64(1, 2L)|] [|StructOfIntInt64(5, 2L)|]

    testCase "Struct prop get" <| fun _ ->
        let command =
            <@
                fun (range: Range1D) (buf:  ClArray<StructOfIntInt64>) ->
                    if range.GlobalID0 = 0 then
                        let mutable y = buf.[0]
                        y.X <- y.X + 3
                        buf.[0] <- y
            @>

        checkResult command [|StructOfIntInt64(1, 2L); StructOfIntInt64(3, 4L)|]
                            [|StructOfIntInt64(4, 2L); StructOfIntInt64(3, 4L)|]
]

// let nestedTypesTests = testList "" [

// ]

let tests =
    testList "Tests on composite types" [
        // tupleTestCases
        // recordTestCases
        structTests
    ]
    |> testSequenced
