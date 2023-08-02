namespace Brahma.FSharp.OpenCL.Translator.QuotationTransformers

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
        | DerivedPatterns.SpecificCall <@ atomic @> (_, _, [ DerivedPatterns.Lambdas _ ]) -> Some()
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
    let predicate onType =
        onType = typeof<int>
        || onType = typeof<uint32>
        ||
        // base
        onType = typeof<int64>
        || onType = typeof<uint64>

    let inline private atomicAdd (p: _ ref) v = (+) p.Value v
    let inline private atomicSub (p: _ ref) v = (-) p.Value v
    let inline private atomicInc (p: _ ref) = inc p.Value
    let inline private atomicDec (p: _ ref) = dec p.Value
    let inline private atomicXchg (p: _ ref) v = xchg p.Value v
    let inline private atomicCmpxchg (p: _ ref) cmp v = cmpxchg p.Value cmp v
    let inline private atomicMin (p: _ ref) v = min p.Value v
    let inline private atomicMax (p: _ ref) v = max p.Value v
    let inline private atomicAnd (p: _ ref) v = (&&&) p.Value v
    let inline private atomicOr (p: _ ref) v = (|||) p.Value v
    let inline private atomicXor (p: _ ref) v = (^^^) p.Value v

    let private atomicAddInfo =
        (Utils.getMethodInfoOfCall <@ atomicAdd @>).GetGenericMethodDefinition()

    let private atomicSubInfo =
        (Utils.getMethodInfoOfCall <@ atomicSub @>).GetGenericMethodDefinition()

    let private atomicIncInfo =
        (Utils.getMethodInfoOfCall <@ atomicInc @>).GetGenericMethodDefinition()

    let private atomicDecInfo =
        (Utils.getMethodInfoOfCall <@ atomicDec @>).GetGenericMethodDefinition()

    let private atomicXchgInfo =
        (Utils.getMethodInfoOfCall <@ atomicXchg @>).GetGenericMethodDefinition()

    let private atomicCmpxchgInfo =
        (Utils.getMethodInfoOfCall <@ atomicCmpxchg @>).GetGenericMethodDefinition()

    let private atomicMinInfo =
        (Utils.getMethodInfoOfCall <@ atomicMin @>).GetGenericMethodDefinition()

    let private atomicMaxInfo =
        (Utils.getMethodInfoOfCall <@ atomicMax @>).GetGenericMethodDefinition()

    let private atomicAndInfo =
        (Utils.getMethodInfoOfCall <@ atomicAnd @>).GetGenericMethodDefinition()

    let private atomicOrInfo =
        (Utils.getMethodInfoOfCall <@ atomicOr @>).GetGenericMethodDefinition()

    let private atomicXorInfo =
        (Utils.getMethodInfoOfCall <@ atomicXor @>).GetGenericMethodDefinition()

    // https://www.khronos.org/registry/OpenCL/sdk/1.2/docs/man/xhtml/atomicFunctions.html
    // TODO(more smart predicate: with flags, etc) see next TODO
    // TODO если устройство не поддерживает атомики для этих типов, то вообще работать не будет
    // нужно либо забить на расширения, либо учитывать параметры девайса
    let mapAtomicCall newApplicationArgs =
        function
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
        | DerivedPatterns.Applications(Specific.Atomic, ([ Patterns.ValidVolatileArg pointerVar ] :: _ as applicationArgs)) ->
            // private vars not supported
            if Set.contains pointerVar nonPrivateVars then
                let newApplicationArgs =
                    applicationArgs |> List.concat |> List.modifyFirst Utils.createRefCall

                mapAtomicCall newApplicationArgs expr
            else
                $"Invalid address space of {pointerVar} var. \
                Atomic operation cannot be executed on variables in private memory"
                |> failwith
        // if volatile arg is invalid
        | DerivedPatterns.Applications(Specific.Atomic, [ invalidVolatileArg ] :: _) ->
            $"Invalid volatile arg of atomic function. Must be `var` of `var.[expr]`, \
            where `var` is variable in local or global memory, but given\n{invalidVolatileArg}"
            |> failwith
        | ExprShape.ShapeVar var -> Expr.Var var
        | ExprShape.ShapeLambda(var, lambda) ->
            let transformedLambda = transform lambda nonPrivateVars
            Expr.Lambda(var, transformedLambda)
        | ExprShape.ShapeCombination(combo, exps) ->
            let transformedList = List.map (fun e -> transform e nonPrivateVars) exps

            ExprShape.RebuildShapeCombination(combo, transformedList)

    let parse (expr: Expr) =
        Helpers.grabVariabls expr |> transform expr
