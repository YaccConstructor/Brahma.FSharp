namespace Brahma.FSharp.OpenCL.Translator.QuotationTransformers

open System
open FSharp.Quotations
open Brahma.FSharp.OpenCL.Translator
open Brahma.FSharp
open Brahma.FSharp.OpenCL.Translator.QuotationTransformers

module Helpers =
    let grabVariabls =
        function
        | DerivedPatterns.Lambdas(args, body) ->
            let globalVars =
                List.concat args |> List.filter Utils.isGlobal |> Set.ofList

            let localVars = Utils.getLocalVars body |> Set.ofList

            Set.union localVars globalVars

        | expr ->
            raise
            <| InvalidKernelException $"Invalid kernel expression. Must be lambda, but given\n{expr}"

module private Specific =
    let (|Unary|_|) q =
        function
        | DerivedPatterns.SpecificCall q (_, onType :: _, [ Patterns.Var _ ]) -> Some onType
        | _ -> None

    let (|Binary|_|) q =
        function
        | DerivedPatterns.SpecificCall q (_, onType :: _, [ Patterns.Var _; Patterns.Var _ ]) -> Some onType
        | _ -> None

    let (|Ternary|_|) q =
        function
        | DerivedPatterns.SpecificCall q (_, onType :: _, [ Patterns.Var _; Patterns.Var _; Patterns.Var _ ]) -> Some onType
        | _ -> None

    let (|Atomic|_|) =
        function
        | DerivedPatterns.SpecificCall <@ atomic @> (_, _, [ DerivedPatterns.Lambdas (_ , body) ]) -> Some body
        | _ -> None

        // move to Specific Call
    let (|CreateRefVar|_|) = function
        | DerivedPatterns.SpecificCall <@ ref @> (_, _, [ Patterns.ValidVolatileArg var ]) -> Some var
        | _ -> None

    let (|AtomicAppArgs|_|) = function
        | _ :: _ :: [ [ CreateRefVar var ] ]
        | _ :: [ [ CreateRefVar var ] ] -> Some var
        | _ -> None

module Atomic =
    module Fun =
        let inline atomicAdd (p: _ ref) v = (+) p.Value v
        let inline atomicSub (p: _ ref) v = (-) p.Value v
        let inline atomicInc (p: _ ref) = inc p.Value
        let inline atomicDec (p: _ ref) = dec p.Value
        let inline atomicXchg (p: _ ref) v = xchg p.Value v
        let inline atomicCmpxchg (p: _ ref) cmp v = cmpxchg p.Value cmp v
        let inline atomicMin (p: _ ref) v = min p.Value v
        let inline atomicMax (p: _ ref) v = max p.Value v
        let inline atomicAnd (p: _ ref) v = (&&&) p.Value v
        let inline atomicOr (p: _ ref) v = (|||) p.Value v
        let inline atomicXor (p: _ ref) v = (^^^) p.Value v

    let atomicAddInfo =
        (Utils.getMethodInfoOfCall <@ Fun.atomicAdd @>).GetGenericMethodDefinition()

    let atomicSubInfo =
        (Utils.getMethodInfoOfCall <@ Fun.atomicSub @>).GetGenericMethodDefinition()

    let atomicIncInfo =
        (Utils.getMethodInfoOfCall <@ Fun.atomicInc @>).GetGenericMethodDefinition()

    let atomicDecInfo =
        (Utils.getMethodInfoOfCall <@ Fun.atomicDec @>).GetGenericMethodDefinition()

    let atomicXchgInfo =
        (Utils.getMethodInfoOfCall <@ Fun.atomicXchg @>).GetGenericMethodDefinition()

    let private atomicCmpxchgInfo =
        (Utils.getMethodInfoOfCall <@ Fun.atomicCmpxchg @>).GetGenericMethodDefinition()

    let private atomicMinInfo =
        (Utils.getMethodInfoOfCall <@ Fun.atomicMin @>).GetGenericMethodDefinition()

    let private atomicMaxInfo =
        (Utils.getMethodInfoOfCall <@ Fun.atomicMax @>).GetGenericMethodDefinition()

    let private atomicAndInfo =
        (Utils.getMethodInfoOfCall <@ Fun.atomicAnd @>).GetGenericMethodDefinition()

    let private atomicOrInfo =
        (Utils.getMethodInfoOfCall <@ Fun.atomicOr @>).GetGenericMethodDefinition()

    let private atomicXorInfo =
        (Utils.getMethodInfoOfCall <@ Fun.atomicXor @>).GetGenericMethodDefinition()

    let private predicate onType =
        onType = typeof<int>
        || onType = typeof<uint32>
        ||
        // base
        onType = typeof<int64>
        || onType = typeof<uint64>

    // https://www.khronos.org/registry/OpenCL/sdk/1.2/docs/man/xhtml/atomicFunctions.html
    // TODO(more smart predicate: with flags, etc) see next TODO
    // TODO если устройство не поддерживает атомики для этих типов, то вообще работать не будет
    // нужно либо забить на расширения, либо учитывать параметры девайса
    let mapAtomicCall newApplicationArgs = function
        | Specific.Binary <@ (+) @> onType when predicate onType
         -> Expr.Call(atomicAddInfo.MakeGenericMethod(onType, onType, onType), newApplicationArgs)
        | Specific.Binary <@ (-) @> onType when predicate onType ->
            Expr.Call(atomicSubInfo.MakeGenericMethod(onType, onType, onType), newApplicationArgs)
        | Specific.Unary <@ inc @> onType when predicate onType ->
            Expr.Call(atomicIncInfo.MakeGenericMethod(onType, onType), newApplicationArgs)
        | Specific.Unary <@ dec @> onType when predicate onType ->
            Expr.Call(atomicDecInfo.MakeGenericMethod(onType, onType), newApplicationArgs)
        | Specific.Binary <@ xchg @> onType when predicate onType || onType = typeof<float32> ->
            Expr.Call(atomicXchgInfo.MakeGenericMethod(onType), newApplicationArgs)
        | Specific.Ternary <@ cmpxchg @> onType when predicate onType ->
            Expr.Call(atomicCmpxchgInfo.MakeGenericMethod(onType), newApplicationArgs)
        | Specific.Binary <@ min @> onType when predicate onType ->
            Expr.Call(atomicMinInfo.MakeGenericMethod(onType), newApplicationArgs)
        | Specific.Binary <@ max @> onType when predicate onType ->
            Expr.Call(atomicMaxInfo.MakeGenericMethod(onType), newApplicationArgs)
        | Specific.Binary <@ (&&&) @> onType when predicate onType ->
            Expr.Call(atomicAndInfo.MakeGenericMethod(onType), newApplicationArgs)
        | Specific.Binary <@ (|||) @> onType when predicate onType ->
            Expr.Call(atomicOrInfo.MakeGenericMethod(onType), newApplicationArgs)
        | Specific.Binary <@ (^^^) @> onType when predicate onType ->
            Expr.Call(atomicXorInfo.MakeGenericMethod(onType), newApplicationArgs)
        | _ -> failwith "Arbitrary atomics are not supported"

    let rec transform (expr: Expr) nonPrivateVars =
        match expr with
        // Atomic application restriction
        | DerivedPatterns.Applications(Specific.Atomic body, ([ Patterns.ValidVolatileArg pointerVar ] :: _ as applicationArgs)) ->
            // private vars not supported
            if Set.contains pointerVar nonPrivateVars then
                let newApplicationArgs =
                    applicationArgs |> List.concat |> List.modifyFirst Utils.createRefCall

                mapAtomicCall newApplicationArgs body
            else
                $"Invalid address space of {pointerVar} var. \
                Atomic operation cannot be executed on variables in private memory"
                |> ArgumentException
                |> raise
        // if volatile arg is invalid
        | DerivedPatterns.Applications(Specific.Atomic _, [ invalidVolatileArg ] :: _) ->
            $"Invalid volatile arg of atomic function. Must be `var` of `var.[expr]`, \
            where `var` is variable in local or global memory, but given\n{invalidVolatileArg}"
            |> ArgumentException
            |> raise
        | ExprShape.ShapeVar var -> Expr.Var var
        | ExprShape.ShapeLambda(var, lambda) ->
            let transformedLambda = transform lambda nonPrivateVars
            Expr.Lambda(var, transformedLambda)
        | ExprShape.ShapeCombination(combo, exps) ->
            let transformedList = List.map (fun e -> transform e nonPrivateVars) exps

            ExprShape.RebuildShapeCombination(combo, transformedList)

    let parse (expr: Expr) =
        Helpers.grabVariabls expr |> transform expr
