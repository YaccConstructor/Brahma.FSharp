namespace Brahma.FSharp.OpenCL.Translator

open System
open System.Collections.Concurrent
open System.Runtime.InteropServices
open Brahma.FSharp.OpenCL.Translator
open FSharp.Reflection
open System.Runtime.CompilerServices
open System.Runtime.Serialization
open FSharpx.Collections

type StructurePacking =
    {
        Size: int
        Alignment: int
        Members: {| Pack: StructurePacking; Offsets: int|} list
    }

type CustomMarshaller() =
    let typePacking = ConcurrentDictionary<Type, StructurePacking>()

    let typeOffsets = ConcurrentDictionary<Type, int[]>()

    let blittableTypes =
        ConcurrentDictionary<Type, bool>(
            dict [
                typeof<decimal>, false
                typeof<byte>, true
                typeof<sbyte>, true
                typeof<int16>, true
                typeof<uint16>, true
                typeof<int32>, true
                typeof<uint32>, true
                typeof<int64>, true
                typeof<uint64>, true
                typeof<nativeint>, true
                typeof<unativeint>, true
                typeof<single>, true
                typeof<double>, true
            ]
        )

    let (|TupleType|RecordType|UnionType|UserDefinedStructureType|PrimitiveType|) (type': Type) =
        match type' with
        | _ when FSharpType.IsTuple type' -> TupleType
        | _ when FSharpType.IsRecord type' -> RecordType
        | _ when FSharpType.IsUnion type' -> UnionType
        | _ when Utils.hasAttribute<StructAttribute> type' -> UserDefinedStructureType
        | _ -> PrimitiveType

    let (|Tuple|Record|Union|UserDefinedStructure|Primitive|) (structure: obj) =
        // because None is null
        if isNull structure then
            Union
        else
            match structure.GetType() with
            | TupleType -> Tuple
            | RecordType -> Record
            | UnionType -> Union
            | UserDefinedStructureType -> UserDefinedStructure
            | _ -> Primitive

    // TODO issues with multithreading
    member this.GetTypePacking(type': Type) =
        let getAlignment elems =
            elems
            |> List.map (fun pack -> pack.Alignment)
            |> List.max

        let getSize alignment elems  =
            elems
            |> List.fold (fun state x -> Utils.roundUp x.Alignment state + x.Size) 0
            |> Utils.roundUp alignment

        let getOffsets elems =
            elems
            |> List.scan (fun state x -> Utils.roundUp x.Alignment state + x.Size) 0
            |> List.take elems.Length

        let getMembers elems offsets =
            (elems, offsets)
            ||> List.zip
            |> List.map (fun (e, o) -> {| Pack = e; Offsets = o |})

        let getTypePacking (type': Type) =
            let rec go (type': Type) =
                match type' with
                | TupleType ->
                    let elems =
                        FSharpType.GetTupleElements type'
                        |> Array.map go
                        |> Array.toList

                    let alignment = elems |> getAlignment
                    let size = elems |> getSize alignment
                    let offsets = elems |> getOffsets
                    let members = (elems, offsets) ||> getMembers

                    { Size = size; Alignment = alignment; Members = members }

                | RecordType ->
                    let elems =
                        FSharpType.GetRecordFields type'
                        |> Array.map (fun pi -> pi.PropertyType)
                        |> Array.map go
                        |> Array.toList

                    let alignment = elems |> getAlignment
                    let size = elems |> getSize alignment
                    let offsets = elems |> getOffsets
                    let members = (elems, offsets) ||> getMembers

                    { Size = size; Alignment = alignment; Members = members }

                | UnionType ->
                    let tag = go typeof<int>
                    let nonEmptyFieldsTypes =
                        FSharpType.GetUnionCases type'
                        |> Array.map
                            (fun unionCase ->
                                unionCase.GetFields()
                                |> Array.map (fun pi -> pi.PropertyType)
                            )
                        |> Array.filter (fun a -> a.Length <> 0)

                    let unionPacking =
                        if nonEmptyFieldsTypes.Length = 0 then
                            { Size = 0; Alignment = 1; Members = [] }
                        else
                            let packingList =
                                nonEmptyFieldsTypes
                                |> Array.map FSharpType.MakeTupleType
                                |> Array.map this.GetTypePacking
                                |> Array.toList

                            let unionAlignment =
                                packingList
                                |> List.map (fun pack -> pack.Alignment)
                                |> List.max

                            let unionSize =
                                packingList
                                |> List.map (fun pack -> pack.Size)
                                |> List.max

                            { Size = unionSize; Alignment = unionAlignment; Members = [] }

                    let elems = [tag; unionPacking]

                    let alignment = elems |> getAlignment
                    let size = elems |> getSize alignment
                    let offsets = elems |> getOffsets
                    let members = (elems, offsets) ||> getMembers

                    { Size = size; Alignment = alignment; Members = members }

                | UserDefinedStructureType ->
                    let elems =
                        type'.GetFields()
                        |> Array.map (fun fi -> fi.FieldType)
                        |> Array.map go
                        |> Array.toList

                    let alignment = elems |> getAlignment
                    let size = elems |> getSize alignment
                    let offsets = elems |> getOffsets
                    let members = (elems, offsets) ||> getMembers

                    { Size = size; Alignment = alignment; Members = members }

                | PrimitiveType ->
                    let size = Marshal.SizeOf (if type' = typeof<bool> then typeof<BoolHostAlias> else type')
                    let alignment = size
                    { Size = size; Alignment = alignment; Members = [] }

            go type'

        let mutable packing = Unchecked.defaultof<StructurePacking>
        if typePacking.TryGetValue(type', &packing) then
            packing
        else
            packing <- getTypePacking type'
            typePacking.TryAdd(type', packing) |> ignore
            packing

    member this.GetTypeOffsets(type': Type) =
        let getOffsets packing =
            let getFlattenOffsets start packing =
                let offsets = ResizeArray()
                let mutable size = 0

                for pack in packing.Members do
                    let offset = Utils.roundUp pack.Pack.Alignment size
                    offsets.Add (offset + start)
                    size <- offset + pack.Pack.Size

                offsets |> Seq.toList

            let rec loop (packing: StructurePacking) (start: int) = seq {
                match packing.Members with
                | [] -> start
                | _ ->
                    let packingOffsetPairs =
                        getFlattenOffsets start packing
                        |> List.zip packing.Members

                    for (packing, offset) in packingOffsetPairs do
                        yield! loop packing.Pack offset
            }

            loop packing 0 |> Seq.toArray

        let mutable offsets = Unchecked.defaultof<int[]>
        if typeOffsets.TryGetValue(type', &offsets) then
            offsets
        else
            offsets <- getOffsets <| this.GetTypePacking(type')
            typeOffsets.TryAdd(type', offsets) |> ignore
            offsets

    member this.IsBlittable(type': Type) =
        let mutable isBlittable = false
        if blittableTypes.TryGetValue(type', &isBlittable) then
            isBlittable
        // TODO is array check useful here?
        elif type'.IsArray then
            let elem = type'.GetElementType()
            isBlittable <- elem.IsValueType && this.IsBlittable(elem)
            blittableTypes.TryAdd(type', isBlittable) |> ignore
            isBlittable
        else
            try
                let instance = FormatterServices.GetUninitializedObject(type');
                GCHandle.Alloc(instance, GCHandleType.Pinned).Free();
                isBlittable <- true
                // TODO remove code repetition
                blittableTypes.TryAdd(type', isBlittable) |> ignore
                isBlittable
            with _ ->
                isBlittable <- false
                blittableTypes.TryAdd(type', isBlittable) |> ignore
                isBlittable

    member this.WriteToUnmanaged(array: 'a[]) =
        let size = array.Length * this.GetTypePacking(typeof<'a>).Size
        let mem = Marshal.AllocHGlobal size
        this.WriteToUnmanaged(array, mem) |> ignore
        size, mem

    member this.WriteToUnmanaged(array: 'a[], ptr: IntPtr) =
        let rec write start (structure: obj) =
            let offsets = this.GetTypeOffsets(if isNull structure then typeof<int option> else structure.GetType())
            let mutable i = 0
            let rec go (str: obj) =
                match str with
                | Tuple ->
                    let tuple = unbox<ITuple> str
                    let tupleSize = tuple.Length
                    [ 0 .. tupleSize - 1 ] |> List.iter (fun i -> go tuple.[i])

                | Record ->
                    FSharpValue.GetRecordFields str
                    |> Array.iter go

                | Union ->
                    let (case, data) = FSharpValue.GetUnionFields(str, if isNull str then typeof<int option> else str.GetType())
                    go case.Tag

                    if data.Length <> 0 then
                        FSharpValue.MakeTuple(
                            data,
                            FSharpType.MakeTupleType(
                                data |> Array.map (fun o -> if isNull o then typeof<int option> else o.GetType())
                            )
                        )
                        |> write (IntPtr.Add(start, offsets.[i]))

                    i <- i + 1

                | UserDefinedStructure ->
                    str.GetType().GetFields()
                    |> Array.map (fun fi -> fi.GetValue(str))
                    |> Array.iter go

                | Primitive ->
                    let offset = if isNull structure then 0 else offsets.[i]
                    let structure =
                        if str.GetType() = typeof<bool> then
                            box <| Convert.ToByte str
                        else
                            str
                    Marshal.StructureToPtr(structure, IntPtr.Add(start, offset), false)
                    i <- i + 1

            go structure

        Array.Parallel.iteri (fun j item ->
            let pack = this.GetTypePacking(typeof<'a>)
            let start = IntPtr.Add(ptr, j * pack.Size)
            write start item
        ) array

        array.Length * this.GetTypePacking(typeof<'a>).Size

    member this.ReadFromUnmanaged<'a>(ptr: IntPtr, n: int) =
        let array = Array.zeroCreate<'a> n
        this.ReadFromUnmanaged(ptr, array)
        array

    member this.ReadFromUnmanaged<'a>(ptr: IntPtr, array: 'a[]) =
        let rec read start type' =
            let offsets = this.GetTypeOffsets(type')
            let mutable i = 0
            let rec go (type'': Type) =
                match type'' with
                | TupleType ->
                    FSharpType.GetTupleElements type''
                    |> Array.map go
                    |> fun x -> FSharpValue.MakeTuple(x, type'')

                | RecordType ->
                    FSharpType.GetRecordFields type''
                    |> Array.map (fun pi -> pi.PropertyType)
                    |> Array.map go
                    |> fun x -> FSharpValue.MakeRecord(type'', x)

                | UnionType ->
                    let tag = unbox<int> <| go typeof<int>
                    let case = FSharpType.GetUnionCases(type'').[tag]
                    let fields = case.GetFields()

                    let union =
                        if fields.Length = 0 then
                            FSharpValue.MakeUnion(case, [||])
                        else
                            fields
                            |> Array.map (fun pi -> pi.PropertyType)
                            |> FSharpType.MakeTupleType
                            |> read (IntPtr.Add(start, offsets.[i]))
                            |> FSharpValue.GetTupleFields
                            |> fun tupleFields -> FSharpValue.MakeUnion(case, tupleFields)
                    i <- i + 1
                    union

                | UserDefinedStructureType ->
                    let inst = Activator.CreateInstance(type'')
                    type''.GetFields()
                    |> Array.map (fun fi -> fi, go fi.FieldType)
                    |> Array.iter (fun (fi, value) -> fi.SetValue(inst, value))

                    inst

                | PrimitiveType ->
                    let offset = offsets.[i]
                    let structure =
                        Marshal.PtrToStructure(
                            IntPtr.Add(start, offset),
                            if type'' = typeof<bool> then typeof<BoolHostAlias> else type''
                        )
                    let structure =
                        if type'' = typeof<bool> then
                            box <| Convert.ToBoolean structure
                        else
                            structure
                    i <- i + 1
                    structure

            go type'

        Array.Parallel.iteri (fun j _ ->
            let start = IntPtr.Add(ptr, j * this.GetTypePacking(typeof<'a>).Size)
            array.[j] <- unbox<'a> <| read start typeof<'a>
        ) array
