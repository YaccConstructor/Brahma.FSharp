module Brahma.FSharp.Tests.Translator.QuatationTransformation.Atomic

open Expecto
open Brahma.FSharp.OpenCL.Translator.QuotationTransformers
open Common
open FSharp.Quotations
open Brahma.FSharp


// let test =
//
//     [
//         testCase "Multiple local values in atomic operations"
//         <| fun () ->
//             let kernel =
//                 <@
//                     fun (ndRange: Range1D) (v: int) ->
//                         let mutable firstMaxIndex = local ()
//                         let mutable secondMaxIndex = local ()
//                         let mutable value = local ()
//
//                         if ndRange.LocalID0 = 0 then
//                             firstMaxIndex <- 0
//                             secondMaxIndex <- 0
//                             value <- v
//
//                         barrierLocal ()
//
//                         atomic (max) firstMaxIndex value |> ignore
//                         atomic (max) secondMaxIndex value |> ignore
//                 @>
//
//             Helpers.openclTranslate kernel |> ignore
//     ]
//
// let commonApiTests =
//     [
//         // TODO is it correct?
//         ptestCase "Using atomic in lambda should not raise exception if first parameter passed"
//         <| fun () ->
//             let command =
//                 <@
//                     fun (range: Range1D) (buffer: int[]) ->
//                         let g = atomic (fun x y -> x + 1) buffer.[0]
//                         g 5 |> ignore
//                 @>
//
//             command |> Helpers.openclTranslate |> ignore
//
//         // TODO is it correct?
//         ptestCase "Using atomic in lambda should raise exception if first parameter is argument"
//         <| fun () ->
//             let command =
//                 <@
//                     fun (range: Range1D) (buffer: int[]) ->
//                         let g x y = atomic (+) x y
//                         g buffer.[0] 6 |> ignore
//                 @>
//
//             Expect.throwsT<System.ArgumentException>
//             <| fun () -> command |> Helpers.openclTranslate |> ignore
//             <| "Exception should be thrown"
//     ]

// Atomic parse accept only lambdas
let inline private mapAndCreateTest name source expected =
    let map expr = <@ fun () -> %expr @>

    createMapTestAndCompareAsStrings Atomic.parse name
    <| map source
    <| map expected

let localMapTests =
    [
        mapAndCreateTest "Int add"
        <| <@  let first: int = local ()
               let second: int = local ()

               atomic (+) first second @>
        <| <@ let first: int = local ()
              let second: int = local ()

              Atomic.Fun.atomicAdd (ref first) second @>

        mapAndCreateTest "Int sub"
        <| <@   let first: int = local ()
                let second: int = local ()

           atomic (-) first second @>
        <| <@ let first: int = local ()
              let second: int = local ()

              Atomic.Fun.atomicSub (ref first) second @>

        mapAndCreateTest "Int or"
        <| <@   let first: int = local ()
                let second: int = local ()

           atomic (|||) first second @>
        <| <@ let first: int = local ()
              let second: int = local ()

              Atomic.Fun.atomicOr (ref first) second @>

        mapAndCreateTest "Int and"
        <| <@   let first: int = local ()
                let second: int = local ()

           atomic (&&&) first second @>
        <| <@ let first: int = local ()
              let second: int = local ()

              Atomic.Fun.atomicAnd (ref first) second @>

        mapAndCreateTest "Int dec"
        <| <@  let first: int = local ()

           atomic dec first @>
        <| <@ let first: int = local ()

              Atomic.Fun.atomicDec (ref first) @>

        mapAndCreateTest "Int inc"
        <| <@   let first: int = local ()

           atomic inc first @>
        <| <@ let first: int = local ()

              Atomic.Fun.atomicInc (ref first) @>

        mapAndCreateTest "Int max"
        <| <@   let first: int = local ()
                let second: int = local ()

           atomic max first second @>
        <| <@ let first: int = local ()
              let second: int = local ()

              Atomic.Fun.atomicMax (ref first) second @>

        mapAndCreateTest "Int min"
        <| <@   let first: int = local ()
                let second: int = local ()

           atomic min first second @>
        <| <@ let first: int = local ()
              let second: int = local ()

              Atomic.Fun.atomicMin (ref first) second @>

        mapAndCreateTest "Int cmpxchg"
        <| <@   let first: int = local ()
                let second: int = local ()
                let third: int = local ()

           atomic cmpxchg first second third @>
        <| <@ let first: int = local ()
              let second: int = local ()
              let third: int = local ()

              Atomic.Fun.atomicCmpxchg (ref first) second third @>

        mapAndCreateTest "Int xor"
        <| <@   let first: int = local ()
                let second: int = local ()

           atomic (^^^) first second @>
        <| <@ let first: int = local ()
              let second: int = local ()

              Atomic.Fun.atomicXor (ref first) second @>
    ]
    |> testList "Local"

let private createTest name = createMapTestAndCompareAsStrings Atomic.parse name

let ClCellMapTests =
    [
        createTest "Int add"
        <| <@ fun (first: ClCell<int>) (second: ClCell<int>) ->  atomic (+) first.Value second.Value @>
        <| <@ fun (first: ClCell<int>) (second: ClCell<int>) -> Atomic.Fun.atomicAdd (ref first.Value) second.Value @>

        createTest "Int sub"
        <| <@  fun (first: ClCell<int>) (second: ClCell<int>) ->  atomic (-) first.Value second.Value  @>
        <| <@ fun (first: ClCell<int>) (second: ClCell<int>) -> Atomic.Fun.atomicSub (ref first.Value) second.Value @>

        createTest "Int or"
        <| <@  fun (first: ClCell<int>) (second: ClCell<int>)->  atomic (|||) first.Value second.Value @>
        <| <@ fun (first: ClCell<int>) (second: ClCell<int>) -> Atomic.Fun.atomicOr (ref first.Value) second.Value @>

        createTest "Int and"
        <| <@  fun (first: ClCell<int>) (second: ClCell<int>) ->  atomic (&&&) first.Value second.Value @>
        <| <@ fun (first: ClCell<int>) (second: ClCell<int>) -> Atomic.Fun.atomicAnd (ref first.Value) second.Value @>

        createTest "Int dec"
        <| <@  fun (first: ClCell<int>) ->  atomic dec first.Value  @>
        <| <@ fun (first: ClCell<int>) -> Atomic.Fun.atomicDec (ref first.Value) @>

        createTest "Int inc"
        <| <@ fun (first: ClCell<int>) ->  atomic inc first.Value @>
        <| <@ fun (first: ClCell<int>) -> Atomic.Fun.atomicInc (ref first.Value) @>

        createTest "Int max"
        <| <@ fun (first: ClCell<int>) (second: ClCell<int>) -> atomic max first.Value second.Value @>
        <| <@ fun (first: ClCell<int>) (second: ClCell<int>) -> Atomic.Fun.atomicMax (ref first.Value) second.Value @>

        createTest "Int min"
        <| <@ fun (first: ClCell<int>) (second: ClCell<int>) ->  atomic min first.Value second.Value @>
        <| <@ fun (first: ClCell<int>) (second: ClCell<int>) -> Atomic.Fun.atomicMin (ref first.Value) second.Value @>

        createTest "Int cmpxchg"
        <| <@ fun (first: ClCell<int>) (second: ClCell<int>) (third: ClCell<int>) -> atomic cmpxchg first.Value second.Value third.Value @>
        <| <@ fun (first: ClCell<int>) (second: ClCell<int>) (third: ClCell<int>) -> Atomic.Fun.atomicCmpxchg (ref first.Value) second.Value third.Value @>

        createTest "Int xor"
        <| <@  fun (first: ClCell<int>) (second: ClCell<int>) -> atomic (^^^) first.Value second.Value @>
        <| <@ fun (first: ClCell<int>) (second: ClCell<int>) -> Atomic.Fun.atomicXor (ref first.Value) second.Value @>
    ]
    |> testList "ClCell"

let ClArrayMapTests =
    [
        createTest "Int add"
        <| <@ fun (first: ClArray<int>) (second: ClArray<int>) ->  atomic (+) first.[0] second.[0] @>
        <| <@ fun (first: ClArray<int>) (second: ClArray<int>) -> Atomic.Fun.atomicAdd (ref first.[0]) second.[0] @>

        createTest "Int sub"
        <| <@  fun (first: ClArray<int>) (second: ClArray<int>) ->  atomic (-) first.[0] second.[0]  @>
        <| <@ fun (first: ClArray<int>) (second: ClArray<int>) -> Atomic.Fun.atomicSub (ref first.[0]) second.[0] @>

        createTest "Int or"
        <| <@  fun (first: ClArray<int>) (second: ClArray<int>)->  atomic (|||) first.[0] second.[0] @>
        <| <@ fun (first: ClArray<int>) (second: ClArray<int>) -> Atomic.Fun.atomicOr (ref first.[0]) second.[0] @>

        createTest "Int and"
        <| <@  fun (first: ClArray<int>) (second: ClArray<int>) ->  atomic (&&&) first.[0] second.[0] @>
        <| <@ fun (first: ClArray<int>) (second: ClArray<int>) -> Atomic.Fun.atomicAnd (ref first.[0]) second.[0] @>

        createTest "Int dec"
        <| <@  fun (first: ClArray<int>) ->  atomic dec first.[0]  @>
        <| <@ fun (first: ClArray<int>) -> Atomic.Fun.atomicDec (ref first.[0]) @>

        createTest "Int inc"
        <| <@ fun (first: ClArray<int>) ->  atomic inc first.[0] @>
        <| <@ fun (first: ClArray<int>) -> Atomic.Fun.atomicInc (ref first.[0]) @>

        createTest "Int max"
        <| <@ fun (first: ClArray<int>) (second: ClArray<int>) -> atomic max first.[0] second.[0] @>
        <| <@ fun (first: ClArray<int>) (second: ClArray<int>) -> Atomic.Fun.atomicMax (ref first.[0]) second.[0] @>

        createTest "Int min"
        <| <@ fun (first: ClArray<int>) (second: ClArray<int>) ->  atomic min first.[0] second.[0] @>
        <| <@ fun (first: ClArray<int>) (second: ClArray<int>) -> Atomic.Fun.atomicMin (ref first.[0]) second.[0] @>

        createTest "Int cmpxchg"
        <| <@ fun (first: ClArray<int>) (second: ClArray<int>) (third: ClArray<int>) -> atomic cmpxchg first.[0] second.[0] third.[0] @>
        <| <@ fun (first: ClArray<int>) (second: ClArray<int>) (third: ClArray<int>) -> Atomic.Fun.atomicCmpxchg (ref first.[0]) second.[0] third.[0] @>

        createTest "Int xor"
        <| <@  fun (first: ClArray<int>) (second: ClArray<int>) -> atomic (^^^) first.[0] second.[0] @>
        <| <@ fun (first: ClArray<int>) (second: ClArray<int>) -> Atomic.Fun.atomicXor (ref first.[0]) second.[0] @>
    ]
    |> testList "ClArray"

let inline createTestWithException<'a when 'a :> exn> name = createTestWithException<Expr, 'a> (Atomic.parse >> ignore) name

let common =
    [
        createTestWithException<System.ArgumentException>
        <| "Using atomic in lambda should raise exception if first parameter is argument"
        <| <@ fun (buffer: int[]) -> let g x y = atomic (+) x y in g buffer.[0] 6 |> ignore @>

        test "Multiple local values in atomic operations" {
            <@
                 fun (ndRange: Range1D) (v: int) ->
                     let mutable firstMaxIndex = local ()
                     let mutable secondMaxIndex = local ()
                     let mutable value = local ()

                     if ndRange.LocalID0 = 0 then
                         firstMaxIndex <- 0
                         secondMaxIndex <- 0
                         value <- v

                     barrierLocal ()

                     atomic max firstMaxIndex value |> ignore
                     atomic max secondMaxIndex value |> ignore
             @>
            |> Atomic.parse |> ignore
        }

    ]
    |> testList "Common"

let tests = testList "Atomic" [ localMapTests; ClArrayMapTests; ClCellMapTests; common ]
