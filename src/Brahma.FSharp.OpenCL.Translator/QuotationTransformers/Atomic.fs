namespace Brahma.FSharp.OpenCL.Translator.QuotationTransformers

open FSharp.Quotations
open Brahma.FSharp.OpenCL.Translator
open Brahma.FSharp.OpenCL.Shared
open FSharp.Core.LanguagePrimitives
open System.Collections.Generic
open Brahma.FSharp

type Mutex = int

type AddressQual =
    | GlobalQ
    | LocalQ

[<AutoOpen>]
module AtomicProcessing =
    let atomicProcessing = StateBuilder<Map<Var, Var>>()

module Atomic =
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

    let private modifyFirstOfList f lst =
        match lst with
        | x :: tail -> f x :: tail
        | _ -> invalidArg "lst" "List should not be empty"

    let private modifyFirstOfListList f lst =
        match lst with
        | [ x ] :: tail -> [ f x ] :: tail
        | _ -> invalidArg "lst" "List should not be empty"

    let private getFirstOfListListWith f lst =
        match lst with
        | [ x ] :: _ -> f x
        | _ -> invalidArg "lst" "List should not be empty"

    // TODO(test)
    let grabVariableAddresses =
        function
        | DerivedPatterns.Lambdas(args, body) ->
            let kernelArgs = List.concat args

            let vars = Dictionary<Var, AddressQual>()

            kernelArgs
            |> List.filter Utils.isGlobal
            |> List.iter (fun v -> vars.Add(v, GlobalQ))

            let rec traverse =
                function
                // TODO(Note: precomputation in specificCall, make static?)
                | Patterns.Let(var, DerivedPatterns.SpecificCall <@ local @> _, body)
                | Patterns.Let(var, DerivedPatterns.SpecificCall <@ localArray @> _, body) ->
                    vars.Add(var, LocalQ)
                    traverse body

                | ExprShape.ShapeVar _ -> ()
                | ExprShape.ShapeLambda(_, lambda) -> traverse lambda
                | ExprShape.ShapeCombination(_, exp) -> List.iter traverse exp

            traverse body

            vars |> Seq.map (|KeyValue|) |> Map.ofSeq

        | expr ->
            raise
            <| InvalidKernelException $"Invalid kernel expression. Must be lambda, but given\n{expr}"

    let rec private transformAtomicsAndCollectPointerVars (expr: Expr) nonPrivateVars =
        atomicProcessing {
            match expr with
            | DerivedPatterns.Applications(DerivedPatterns.SpecificCall <@ atomic @> (_,
                                                                                      _,
                                                                                      [ DerivedPatterns.Lambdas(lambdaArgs, lambdaBody) ]),
                                           // atomic application restriction
                                           ([ Patterns.ValidVolatileArg pointerVar as volatileArg ] :: _ as applicationArgs)) when
                nonPrivateVars |> Map.containsKey pointerVar
                -> // private vars not supported

                let newApplicationArgs =
                    applicationArgs |> List.concat |> modifyFirstOfList Utils.createRefCall

                // https://www.khronos.org/registry/OpenCL/sdk/1.2/docs/man/xhtml/atomicFunctions.html
                match lambdaBody with
                | DerivedPatterns.SpecificCall <@ (+) @> (_, onType :: _, [ Patterns.Var _; Patterns.Var _ ]) when
                    onType = typeof<int>
                    || onType = typeof<uint32>
                    ||
                    // base
                    onType = typeof<int64>
                    || onType = typeof<uint64>
                    ->
                    return Expr.Call(atomicAddInfo.MakeGenericMethod(onType, onType, onType), newApplicationArgs)

                | DerivedPatterns.SpecificCall <@ (-) @> (_, onType :: _, [ Patterns.Var _; Patterns.Var _ ]) when
                    onType = typeof<int>
                    || onType = typeof<uint32>
                    ||
                    // base
                    onType = typeof<int64>
                    || onType = typeof<uint64>
                    ->
                    return Expr.Call(atomicSubInfo.MakeGenericMethod(onType, onType, onType), newApplicationArgs)

                | DerivedPatterns.SpecificCall <@ inc @> (_, onType :: _, [ Patterns.Var _ ]) when
                    onType = typeof<int>
                    || onType = typeof<uint32>
                    ||
                    // base
                    onType = typeof<int64>
                    || onType = typeof<uint64>
                    ->
                    return Expr.Call(atomicIncInfo.MakeGenericMethod(onType, onType), newApplicationArgs)

                | DerivedPatterns.SpecificCall <@ dec @> (_, onType :: _, [ Patterns.Var _ ]) when
                    onType = typeof<int>
                    || onType = typeof<uint32>
                    ||
                    // base
                    onType = typeof<int64>
                    || onType = typeof<uint64>
                    ->
                    return Expr.Call(atomicDecInfo.MakeGenericMethod(onType, onType), newApplicationArgs)

                | DerivedPatterns.SpecificCall <@ xchg @> (_, onType :: _, [ Patterns.Var _; Patterns.Var _ ]) when
                    onType = typeof<int>
                    || onType = typeof<uint32>
                    || onType = typeof<float32>
                    ||
                    // base
                    onType = typeof<int64>
                    || onType = typeof<uint64>
                    ->
                    return Expr.Call(atomicXchgInfo.MakeGenericMethod(onType), newApplicationArgs)

                | DerivedPatterns.SpecificCall <@ cmpxchg @> (_, onType :: _, [ Patterns.Var _; Patterns.Var _; Patterns.Var _ ]) when
                    onType = typeof<int>
                    || onType = typeof<uint32>
                    ||
                    // base
                    onType = typeof<int64>
                    || onType = typeof<uint64>
                    ->
                    return Expr.Call(atomicCmpxchgInfo.MakeGenericMethod(onType), newApplicationArgs)

                | DerivedPatterns.SpecificCall <@ min @> (_, onType :: _, [ Patterns.Var _; Patterns.Var _ ]) when
                    onType = typeof<int>
                    || onType = typeof<uint32>
                    ||
                    // TODO если устройство не поддерживает атомики для этих типов, то вообще работать не будет
                    // нужно либо забить на расширения, либо учитывать параметры девайса
                    // extended
                    onType = typeof<int64>
                    || onType = typeof<uint64>
                    ->
                    return Expr.Call(atomicMinInfo.MakeGenericMethod(onType), newApplicationArgs)

                | DerivedPatterns.SpecificCall <@ max @> (_, onType :: _, [ Patterns.Var _; Patterns.Var _ ]) when
                    onType = typeof<int>
                    || onType = typeof<uint32>
                    ||
                    // extended
                    onType = typeof<int64>
                    || onType = typeof<uint64>
                    ->
                    return Expr.Call(atomicMaxInfo.MakeGenericMethod(onType), newApplicationArgs)

                | DerivedPatterns.SpecificCall <@ (&&&) @> (_, onType :: _, [ Patterns.Var _; Patterns.Var _ ]) when
                    onType = typeof<int>
                    || onType = typeof<uint32>
                    ||
                    // extended
                    onType = typeof<int64>
                    || onType = typeof<uint64>
                    ->
                    return Expr.Call(atomicAndInfo.MakeGenericMethod(onType), newApplicationArgs)

                | DerivedPatterns.SpecificCall <@ (|||) @> (_, onType :: _, [ Patterns.Var _; Patterns.Var _ ]) when
                    onType = typeof<int>
                    || onType = typeof<uint32>
                    ||
                    // extended
                    onType = typeof<int64>
                    || onType = typeof<uint64>
                    ->
                    return Expr.Call(atomicOrInfo.MakeGenericMethod(onType), newApplicationArgs)

                | DerivedPatterns.SpecificCall <@ (^^^) @> (_, onType :: _, [ Patterns.Var _; Patterns.Var _ ]) when
                    onType = typeof<int>
                    || onType = typeof<uint32>
                    ||
                    // extended
                    onType = typeof<int64>
                    || onType = typeof<uint64>
                    ->
                    return Expr.Call(atomicXorInfo.MakeGenericMethod(onType), newApplicationArgs)

                | lambdaBody ->
                    let collectedLambdaTypes =
                        lambdaArgs
                        |> List.collect id
                        |> List.map (fun var -> var.Type)
                        |> fun args -> args @ [ lambdaBody.Type ]

                    (* baseFunc *)

                    let baseFuncType = collectedLambdaTypes |> Utils.makeLambdaType

                    let baseFuncVar = Var("baseFunc", baseFuncType)

                    let baseFuncArgs = lambdaArgs

                    let baseFuncBody =
                        match lambdaBody with
                        | DerivedPatterns.SpecificCall <@ inc @> (_, onType :: _, [ Patterns.Var p ]) ->
                            Expr.Call(
                                Utils.makeGenericMethodCall [ onType; onType; onType ] <@ (+) @>,
                                [
                                    Expr.Var p
                                    Expr.Call(Utils.makeGenericMethodCall [ onType ] <@ GenericOne<int> @>, List.empty)
                                ]
                            )

                        | DerivedPatterns.SpecificCall <@ dec @> (_, onType :: _, [ Patterns.Var p ]) ->
                            Expr.Call(
                                Utils.makeGenericMethodCall [ onType; onType; onType ] <@ (-) @>,
                                [
                                    Expr.Var p
                                    Expr.Call(Utils.makeGenericMethodCall [ onType ] <@ GenericOne<int> @>, List.empty)
                                ]
                            )

                        | DerivedPatterns.SpecificCall <@ xchg @> (_, _, [ Patterns.Var p; Patterns.Var value ]) -> Expr.Var value

                        | DerivedPatterns.SpecificCall <@ cmpxchg @> (_,
                                                                      onType :: _,
                                                                      [ Patterns.Var p; Patterns.Var cmp; Patterns.Var value ]) ->
                            Expr.IfThenElse(
                                Expr.Call(Utils.makeGenericMethodCall [ onType ] <@ (=) @>, [ Expr.Var p; Expr.Var cmp ]),
                                Expr.Var value,
                                Expr.Var p
                            )

                        | _ -> lambdaBody

                    (* atomicFunc*)

                    let atomicFuncType =
                        collectedLambdaTypes
                        |> modifyFirstOfList (fun x -> typeof<ref<_>>.GetGenericTypeDefinition().MakeGenericType(x))
                        |> Utils.makeLambdaType

                    let atomicFuncVar = Var("atomicFunc", atomicFuncType)

                    let atomicFuncArgs =
                        baseFuncArgs
                        |> modifyFirstOfListList (fun x ->
                            Var(x.Name, typeof<ref<_>>.GetGenericTypeDefinition().MakeGenericType(x.Type), x.IsMutable))

                    let! state = State.get

                    let mutexVar =
                        match state |> Map.tryFind pointerVar with
                        // if mutex var already exists (if 2 or more atomic op on the same data)
                        | Some mVar -> mVar
                        | None ->
                            Var(
                                pointerVar.Name + "Mutex",
                                if nonPrivateVars.[pointerVar] = GlobalQ then
                                    typeof<IBuffer<Mutex>>
                                elif pointerVar.Type.IsArray then
                                    typeof<Mutex[]>
                                else
                                    typeof<Mutex>
                            )

                    do! State.modify (fun state -> state |> Map.add pointerVar mutexVar)

                    let atomicFuncBody =
                        let mutex =
                            match volatileArg with
                            | Patterns.PropertyGet(Some(Patterns.Var v), propInfo, args) when
                                v.Type.Name.ToLower().StartsWith ClArray_
                                && propInfo.Name.ToLower().StartsWith "item"
                                ->

                                Expr.PropertyGet(Expr.Var mutexVar, typeof<IBuffer<Mutex>>.GetProperty ("Item"), args)

                            | Patterns.PropertyGet(Some(Patterns.Var v), propInfo, args) when
                                v.Type.Name.ToLower().StartsWith ClCell_
                                && propInfo.Name.ToLower().StartsWith "value"
                                ->

                                Expr.PropertyGet(Expr.Var mutexVar, typeof<IBuffer<Mutex>>.GetProperty ("Item"), [ Expr.Value 0 ])

                            | Patterns.Var _ -> Expr.Var mutexVar

                            | DerivedPatterns.SpecificCall <@ IntrinsicFunctions.GetArray @> (_, _, [ Patterns.Var _; idx ]) ->
                                Expr.Call(Utils.getMethodInfoOfCall <@ IntrinsicFunctions.GetArray<Mutex> @>, [ Expr.Var mutexVar; idx ])

                            | _ -> failwith "Invalid volatile argument. This exception should never occur :)"
                            |> Utils.createRefCall

                        let baseFuncApplicaionArgs =
                            atomicFuncArgs
                            |> List.map (List.map Expr.Var)
                            |> modifyFirstOfListList Utils.createDereferenceCall

                        let oldValueVar =
                            Var("oldValue", getFirstOfListListWith (fun (x: Var) -> x.Type.GenericTypeArguments.[0]) atomicFuncArgs, true)

                        Expr.Let(
                            oldValueVar,
                            Expr.DefaultValue
                            <| getFirstOfListListWith (fun (x: Var) -> x.Type.GenericTypeArguments.[0]) atomicFuncArgs,
                            Expr.Sequential(
                                <@@
                                    let mutable flip = 0
                                    let mutable flag = true

                                    while flag do
                                        let old = atomicXchg %%mutex (1 - flip)

                                        if old = flip then
                                            %% Expr.VarSet(oldValueVar, getFirstOfListListWith id baseFuncApplicaionArgs)

                                            %%(Utils.createReferenceSetCall
                                               <| getFirstOfListListWith Expr.Var atomicFuncArgs
                                               <| Expr.Applications(Expr.Var baseFuncVar, baseFuncApplicaionArgs))

                                            flag <- false

                                        flip <- 1 - flip
                                @@>,
                                Expr.Var oldValueVar
                            )
                        )

                    return
                        Expr.Let(
                            baseFuncVar,
                            Expr.Lambdas(baseFuncArgs, baseFuncBody),
                            Expr.Let(
                                atomicFuncVar,
                                Expr.Lambdas(atomicFuncArgs, atomicFuncBody),
                                Expr.Applications(Expr.Var atomicFuncVar, newApplicationArgs |> List.map List.singleton)
                            )
                        )

            // if pointer var in private memory
            | DerivedPatterns.Applications(DerivedPatterns.SpecificCall <@ atomic @> (_,
                                                                                      _,
                                                                                      [ DerivedPatterns.Lambdas(lambdaArgs, lambdaBody) ]),
                                           ([ Patterns.ValidVolatileArg pointerVar ] :: _ as applicationArgs)) when
                nonPrivateVars |> Map.containsKey pointerVar |> not
                ->
                return
                    failwithf
                        $"Invalid address space of {pointerVar} var. \
                Atomic operaion cannot be executed on variables in private memmory"

            // if volatile arg is invalid
            | DerivedPatterns.Applications(DerivedPatterns.SpecificCall <@ atomic @> (_, _, [ DerivedPatterns.Lambdas _ ]),
                                           [ invalidVolatileArg ] :: _) ->
                return
                    failwithf
                        $"Invalid volatile arg of atomic function. Must be `var` of `var.[expr]`, \
                where `var` is variable in local or global memory, but given\n{invalidVolatileArg}"

            | ExprShape.ShapeVar var -> return Expr.Var var
            | ExprShape.ShapeLambda(var, lambda) ->
                let! transformedLambda = transformAtomicsAndCollectPointerVars lambda nonPrivateVars
                return Expr.Lambda(var, transformedLambda)
            | ExprShape.ShapeCombination(combo, exprs) ->
                let! transformedList =
                    exprs
                    |> List.map (fun e -> transformAtomicsAndCollectPointerVars e nonPrivateVars)
                    |> State.collect

                return ExprShape.RebuildShapeCombination(combo, transformedList)
        }

    let private insertMutexVars (expr: Expr) =
        atomicProcessing {
            let! pointerVarToMutexVarMap = State.get

            match expr with
            | DerivedPatterns.Lambdas(args, body) ->
                let args = List.collect id args
                let newArgs = ResizeArray args

                // Set global args
                pointerVarToMutexVarMap
                |> Map.iter (fun var mutexVar ->
                    if args |> List.contains var then
                        newArgs.Add mutexVar)

                // Set local args
                let rec go expr =
                    match expr with
                    | Patterns.Let(var, (DerivedPatterns.SpecificCall <@ local @> (_, _, args) as letExpr), inExpr) ->
                        Expr.Let(
                            var,
                            letExpr,
                            match pointerVarToMutexVarMap |> Map.tryFind var with
                            | Some mutexVar -> failwith "Atomic local non-array variables is not supported yet"
                            // Expr.Let(
                            //     mutexVar,
                            //     Expr.Call(Utils.getMethodInfoOfLambda <@ local<int> @>, args),
                            //     inExpr
                            // )
                            | None -> inExpr
                        )
                    | Patterns.Let(var, (DerivedPatterns.SpecificCall <@ localArray @> (_, _, args) as letExpr), inExpr) ->
                        Expr.Let(
                            var,
                            letExpr,
                            match pointerVarToMutexVarMap |> Map.tryFind var with
                            | Some mutexVar ->
                                Expr.Let(
                                    mutexVar,
                                    Expr.Call(Utils.getMethodInfoOfCall <@ localArray<int> @>, args),
                                    Expr.Sequential(
                                        <@@
                                            if Anchors._localID0 = 0 then
                                                %%(let i = Var("i", typeof<int>, true)

                                                   Expr.ForIntegerRangeLoop(
                                                       i,
                                                       Expr.Value 0,
                                                       <@@ (%%args.[0]: int) - 1 @@>,
                                                       Expr.Call(
                                                           Utils.getMethodInfoOfCall <@ IntrinsicFunctions.SetArray<Mutex> @>,
                                                           [ Expr.Var mutexVar; Expr.Var i; Expr.Value 0 ]
                                                       )
                                                   ))

                                            barrierLocal ()
                                        @@>,
                                        inExpr
                                    )
                                )
                            | None -> inExpr
                        )

                    | ExprShape.ShapeVar var -> Expr.Var var
                    | ExprShape.ShapeLambda(var, lambda) -> Expr.Lambda(var, go lambda)
                    | ExprShape.ShapeCombination(combo, exprs) -> ExprShape.RebuildShapeCombination(combo, List.map go exprs)

                return Expr.Lambdas(Seq.toList newArgs |> List.map List.singleton, go body)

            | _ ->
                return
                    raise
                    <| InvalidKernelException $"Invalid kernel expression. Must be lambda, but given\n{expr}"
        }

    let parse (expr: Expr) =
        let nonPrivateVars = grabVariableAddresses expr

        transformAtomicsAndCollectPointerVars expr nonPrivateVars
        >>= insertMutexVars
        |> State.eval Map.empty
