module CompositeTypesTests

open Expecto
open FSharp.Quotations
open FsCheck
open Brahma.FSharp

// Incomplete pattern matching in record deconstruction
#nowarn "667"

[<AutoOpen>]
module Helpers =
    let check<'a when 'a: equality> context (data: 'a[]) (command: int -> Expr<Range1D -> ClArray<'a> -> unit>) =
        let length = data.Length

        let expected = data

        let actual =
            opencl {
                use! buffer = ClArray.toDevice data

                do!
                    runCommand (command length)
                    <| fun it -> it <| Range1D.CreateValid(data.Length, 256) <| buffer

                return! ClArray.toHost buffer
            }
            |> ClTask.runSync context

        "Arrays should be equal" |> Expect.sequenceEqual actual expected

    let message typeName = $"Simple test on `%s{typeName}`"

[<Struct>]
type RecordOfIntInt64 = { X: int; Y: int64 }

[<Struct>]
type RecordOfBoolBool = { X: bool; Y: bool }

[<Struct>]
type GenericRecord<'a, 'b> = { mutable X: 'a; mutable Y: 'b }

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

let tupleTestCases context =
    [
        let inline check data command = check context data command

        let inline command length =
            <@
                fun (gid: int) (buffer: clarray<struct ('a * 'b)>) ->
                    if gid < length then
                        let struct (a, b) = buffer.[gid]
                        buffer.[gid] <- struct (a, b)
            @>

        testProperty (message "struct(int * int)")
        <| fun (data: struct (int * int)[]) ->
            if data.Length <> 0 then
                check
                    data
                    (fun length ->
                        <@ fun (range: Range1D) (buffer: ClArray<_>) -> (%command length) range.GlobalID0 buffer @>
                    )

        testProperty (message "struct(int * int64)")
        <| fun (data: struct (int * int64)[]) ->
            if data.Length <> 0 then
                check
                    data
                    (fun length ->
                        <@ fun (range: Range1D) (buffer: ClArray<_>) -> (%command length) range.GlobalID0 buffer @>
                    )

        testProperty (message "struct(bool * bool")
        <| fun (data: struct (bool * bool)[]) ->
            if data.Length <> 0 then
                check
                    data
                    (fun length ->
                        <@ fun (range: Range1D) (buffer: ClArray<_>) -> (%command length) range.GlobalID0 buffer @>
                    )

        testProperty (message "struct((int * int) * (int * int))")
        <| fun (data: struct ((int * int) * (int * int))[]) ->
            if data.Length <> 0 then
                check
                    data
                    (fun length ->
                        <@ fun (range: Range1D) (buffer: ClArray<_>) -> (%command length) range.GlobalID0 buffer @>
                    )

        testProperty (message "struct((int * int64) * (bool * bool))")
        <| fun (data: struct ((int * int64) * (bool * bool))[]) ->
            if data.Length <> 0 then
                check
                    data
                    (fun length ->
                        <@ fun (range: Range1D) (buffer: ClArray<_>) -> (%command length) range.GlobalID0 buffer @>
                    )

        testProperty (message "struct(RecordOfIntInt64 * RecordOfBoolBool)")
        <| fun (data: struct (RecordOfIntInt64 * RecordOfBoolBool)[]) ->
            if data.Length <> 0 then
                check
                    data
                    (fun length ->
                        <@ fun (range: Range1D) (buffer: ClArray<_>) -> (%command length) range.GlobalID0 buffer @>
                    )

        testProperty (message "struct(GenericRecord<int, int64> * GenericRecord<bool, bool>)")
        <| fun (data: struct (GenericRecord<int, int64> * GenericRecord<bool, bool>)[]) ->
            if data.Length <> 0 then
                check
                    data
                    (fun length ->
                        <@ fun (range: Range1D) (buffer: ClArray<_>) -> (%command length) range.GlobalID0 buffer @>
                    )

        testProperty (message "struct(int * int64 * bool)")
        <| fun (data: struct (int * int64 * bool)[]) ->
            if data.Length <> 0 then
                check data
                <| fun length ->
                    <@
                        fun (range: Range1D) (buffer: ClArray<_>) ->
                            let gid = range.GlobalID0

                            if gid < length then
                                let struct (a1, a2, a3) = buffer.[gid]
                                buffer.[gid] <- struct (a1, a2, a3)
                    @>

        testProperty "Simple test on big tuple (of size 10)"
        <| fun (data: struct (int * int * int * int * int * int * int * int * int * int)[]) ->
            if data.Length <> 0 then
                check data
                <| fun length ->
                    <@
                        fun (range: Range1D) (buffer: ClArray<_>) ->
                            let gid = range.GlobalID0

                            if gid < length then
                                let struct (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = buffer.[gid]
                                buffer.[gid] <- struct (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
                    @>

        testProperty "Test on inner tuples deconstruction"
        <| fun (data: struct ((int * int) * (int * int))[]) ->
            if data.Length <> 0 then
                check data
                <| fun length ->
                    <@
                        fun (range: Range1D) (buffer: ClArray<_>) ->
                            let gid = range.GlobalID0

                            if gid < length then
                                let struct ((a, b), (c, d)) = buffer.[gid]
                                buffer.[gid] <- struct ((a, b), (c, d))
                    @>
    ]

let recordTestCases context =
    [
        let inline check data command = check context data command

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

        testProperty (message "GenericRecord<int, bool>")
        <| fun (data: GenericRecord<int, bool>[]) ->
            if data.Length <> 0 then
                check
                    data
                    (fun length ->
                        <@ fun (range: Range1D) (buffer: ClArray<_>) -> (%command length) range.GlobalID0 buffer @>
                    )

        testProperty (message "GenericRecord<(int * int64), (bool * bool)>")
        <| fun (data: GenericRecord<(int * int64), (bool * bool)>[]) ->
            if data.Length <> 0 then
                check
                    data
                    (fun length ->
                        <@ fun (range: Range1D) (buffer: ClArray<_>) -> (%command length) range.GlobalID0 buffer @>
                    )
    ]

let genGenericStruct<'a, 'b> =
    gen {
        let! x = Arb.generate<'a>
        let! y = Arb.generate<'b>

        return GenericStruct(x, y)
    }

type GenericStructGenerator =
    static member GenericStruct() = Arb.fromGen genGenericStruct

let structTests context =
    [
        let inline check data command = check context data command

        let inline checkResult cmd input expected = RuntimeTests.Helpers.checkResult context cmd input expected

        testCase "Smoke test"
        <| fun _ ->
            let command =
                <@
                    fun (range: Range1D) (buf: ClArray<StructOfIntInt64>) ->
                        if range.GlobalID0 = 0 then
                            let b = buf.[0]
                            buf.[0] <- buf.[1]
                            buf.[1] <- b
                @>

            checkResult
                command
                [| StructOfIntInt64(1, 2L); StructOfIntInt64(3, 4L) |]
                [| StructOfIntInt64(3, 4L); StructOfIntInt64(1, 2L) |]

        testCase "Struct constructor test"
        <| fun _ ->
            let command =
                <@ fun (range: Range1D) (buf: ClArray<StructOfIntInt64>) -> buf.[0] <- StructOfIntInt64(5, 6L) @>

            checkResult
                command
                [| StructOfIntInt64(1, 2L); StructOfIntInt64(3, 4L) |]
                [| StructOfIntInt64(5, 6L); StructOfIntInt64(3, 4L) |]

        testCase "Struct prop set"
        <| fun _ ->
            let command =
                <@
                    fun (range: Range1D) (buf: ClArray<StructOfIntInt64>) ->
                        let mutable y = buf.[0]
                        y.X <- 5
                        buf.[0] <- y
                @>

            checkResult command [| StructOfIntInt64(1, 2L) |] [| StructOfIntInt64(5, 2L) |]

        testCase "Struct prop get"
        <| fun _ ->
            let command =
                <@
                    fun (range: Range1D) (buf: ClArray<StructOfIntInt64>) ->
                        if range.GlobalID0 = 0 then
                            let mutable y = buf.[0]
                            y.X <- y.X + 3
                            buf.[0] <- y
                @>

            checkResult
                command
                [| StructOfIntInt64(1, 2L); StructOfIntInt64(3, 4L) |]
                [| StructOfIntInt64(4, 2L); StructOfIntInt64(3, 4L) |]

        let inline command length =
            <@
                fun (gid: int) (buffer: ClArray<GenericStruct<'a, 'b>>) ->
                    if gid < length then
                        let tmp = buffer.[gid]
                        let x = tmp.X
                        let y = tmp.Y
                        let mutable innerStruct = GenericStruct(x, y)
                        innerStruct.X <- x
                        innerStruct.Y <- y
                        buffer.[gid] <- GenericStruct(innerStruct.X, innerStruct.Y)
            @>

        let config =
            { FsCheckConfig.defaultConfig with
                arbitrary = [ typeof<GenericStructGenerator> ]
            }

        testPropertyWithConfig config (message "GenericStruct<int, bool>")
        <| fun (data: GenericStruct<int, bool>[]) ->
            if data.Length <> 0 then
                check
                    data
                    (fun length ->
                        <@ fun (range: Range1D) (buffer: ClArray<_>) -> (%command length) range.GlobalID0 buffer @>
                    )

        testPropertyWithConfig config (message "GenericStruct<(int * int64), (bool * bool)>")
        <| fun (data: GenericStruct<(int * int64), (bool * bool)>[]) ->
            if data.Length <> 0 then
                check
                    data
                    (fun length ->
                        <@ fun (range: Range1D) (buffer: ClArray<_>) -> (%command length) range.GlobalID0 buffer @>
                    )

        testPropertyWithConfig config (message "GenericStruct<RecordOfIntInt64, RecordOfBoolBool>")
        <| fun (data: GenericStruct<RecordOfIntInt64, RecordOfBoolBool>[]) ->
            if data.Length <> 0 then
                check
                    data
                    (fun length ->
                        <@ fun (range: Range1D) (buffer: ClArray<_>) -> (%command length) range.GlobalID0 buffer @>
                    )
    ]

type SimpleDU =
    | A
    | B of int
    | C of int64 * bool

type GenericDU<'a, 'b> =
    | A
    | B of RecordOfIntInt64
    | C of GenericRecord<'a, 'b>

type EnumDU =
    | A
    | B
    | C

let unionTests context =
    [
        let inline check data command = check context data command

        testProperty (message "Option<GenericRecord<RecordOfIntInt64, RecordOfBoolBool>>")
        <| fun (data: Option<GenericRecord<RecordOfIntInt64, RecordOfBoolBool>>[]) ->
            if data.Length <> 0 then
                check data
                <| fun length ->
                    <@
                        fun (range: Range1D) (buffer: ClArray<_>) ->
                            let gid = range.GlobalID0

                            if gid < length then
                                buffer.[gid] <-
                                    match buffer.[gid] with
                                    | Some { X = x; Y = y } -> Some { X = x; Y = y }
                                    | None -> None
                    @>

        testProperty (message "Option<Option<RecordOfIntInt64>>")
        <| fun (data: Option<Option<RecordOfIntInt64>>[]) ->
            if data.Length <> 0 then
                check data
                <| fun length ->
                    <@
                        fun (range: Range1D) (buffer: ClArray<_>) ->
                            let gid = range.GlobalID0

                            if gid < length then
                                buffer.[gid] <-
                                    match buffer.[gid] with
                                    | Some a ->
                                        match a with
                                        | Some { X = x; Y = y } -> Some(Some { X = x; Y = y })
                                        | None -> Some None
                                    | None -> None
                    @>

        testProperty (message "SimpleDU")
        <| fun (data: SimpleDU[]) ->
            if data.Length <> 0 then
                check data
                <| fun length ->
                    <@
                        fun (range: Range1D) (buffer: ClArray<_>) ->
                            let gid = range.GlobalID0

                            if gid < length then
                                buffer.[gid] <-
                                    match buffer.[gid] with
                                    | SimpleDU.A -> SimpleDU.A
                                    | SimpleDU.B x -> SimpleDU.B x
                                    | SimpleDU.C(x, y) -> SimpleDU.C(x, y)
                    @>

        ptestProperty (message "GenericDU<bool, Option<bool>>")
        <| fun (data: GenericDU<bool, Option<bool>>[]) ->
            if data.Length <> 0 then
                check data
                <| fun length ->
                    <@
                        fun (range: Range1D) (buffer: ClArray<_>) ->
                            let gid = range.GlobalID0

                            if gid < length then
                                buffer.[gid] <-
                                    match buffer.[gid] with
                                    | GenericDU.A -> GenericDU.A
                                    | GenericDU.B x -> GenericDU.B x
                                    | GenericDU.C { X = x; Y = y } ->
                                        match y with
                                        | Some b -> GenericDU.C { X = x; Y = Some b }
                                        | None -> GenericDU.C { X = x; Y = None }
                    @>

        testProperty (message "GenericRecord<Option<int>, Option<int64>>")
        <| fun (data: GenericRecord<Option<int>, Option<int64>>[]) ->
            if data.Length <> 0 then
                check data
                <| fun length ->
                    <@
                        fun (range: Range1D) (buffer: ClArray<_>) ->
                            let gid = range.GlobalID0

                            if gid < length then
                                buffer.[gid] <-
                                    match buffer.[gid] with
                                    | { X = Some x; Y = Some y } -> { X = Some x; Y = Some y }
                                    | { X = Some x; Y = None } -> { X = Some x; Y = None }
                                    | { X = None; Y = Some y } -> { X = None; Y = Some y }
                                    | { X = None; Y = None } -> { X = None; Y = None }
                    @>

        testProperty (message "EnumDU")
        <| fun (data: EnumDU[]) ->
            if data.Length <> 0 then
                check data
                <| fun length ->
                    <@
                        fun (range: Range1D) (buffer: ClArray<_>) ->
                            let gid = range.GlobalID0

                            if gid < length then
                                buffer.[gid] <-
                                    match buffer.[gid] with
                                    | EnumDU.A -> EnumDU.A
                                    | EnumDU.B -> EnumDU.B
                                    | EnumDU.C -> EnumDU.C
                    @>
    ]

let tests context =
    [
        testList "Tuple tests" << tupleTestCases
        testList "Record tests" << recordTestCases
        testList "Struct tests" << structTests
        testList "Union tests" << unionTests
    ]
    |> List.map (fun testFixture -> testFixture context)
