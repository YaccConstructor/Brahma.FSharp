namespace Brahma.FSharp.Benchmarks

open BenchmarkDotNet.Attributes
open FsCheck

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

module Generators =
    type MyGenerators =
        static member StructOfIntInt64() =
            { new Arbitrary<StructOfIntInt64>() with
                override x.Generator =
                    gen {
                        let! x = Arb.generate<int>
                        let! y = Arb.generate<int64>

                        return StructOfIntInt64(x, y)
                    }
                override x.Shrinker t = Seq.empty
            }

        static member GenericStructOfIntInt64() =
            { new Arbitrary<GenericStruct<'a, 'b>>() with
                override x.Generator =
                    gen {
                        let! x = Arb.generate<'a>
                        let! y = Arb.generate<'b>

                        return GenericStruct(x, y)
                    }
                override x.Shrinker t = Seq.empty
            }

[<AbstractClass>]
type TransferBenchmarks<'a>() =
    member val HostArray = Unchecked.defaultof<'a[]> with get, set

    [<ParamsSource("ArrayLengthProvider")>]
    member val ArrayLength = 0 with get, set

    [<GlobalSetup>]
    member this.InitializeHostArray() =
        Arb.register<Generators.MyGenerators>() |> ignore
        this.HostArray <-
            Arb.generate<'a>
            |> Gen.sample 0 this.ArrayLength
            |> Array.ofList

    static member ArrayLengthProvider =
        seq {
            1000
            1_000_000
        }
