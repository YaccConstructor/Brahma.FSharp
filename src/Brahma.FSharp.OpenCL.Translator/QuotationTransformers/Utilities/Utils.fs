namespace Brahma.FSharp.OpenCL.Translator.QuotationTransformers

open FSharp.Reflection
open FSharp.Quotations
open Brahma.FSharp
open Brahma.FSharp.OpenCL.Translator

module Utils =
    let rec getFunctionArgTypes (funType: System.Type) =
        let argType, retType = FSharpType.GetFunctionElements(funType)

        match retType with
        | _ when FSharpType.IsFunction retType -> argType :: getFunctionArgTypes retType
        | _ -> [ argType ]

    let makeFunctionType (retType: System.Type) (argTypes: List<System.Type>) =
        List.foldBack (fun tp acc -> FSharpType.MakeFunctionType(tp, acc)) argTypes retType

    let makeLambdaType types =
        List.reduceBack (fun domain range -> FSharpType.MakeFunctionType(domain, range)) types

    let makeLambdaExpr (args: Var list) (body: Expr) =
        List.foldBack (fun var expr -> Expr.Lambda(var, expr)) args body

    let makeApplicationExpr (head: Expr) (expressions: Expr list) =
        List.fold (fun l r -> Expr.Application(l, r)) head expressions

    let rec collectLambdaArguments =
        function
        | DerivedPatterns.Lambdas(var, _) -> List.concat var
        | _ -> []

    let rec collectFreeVarsWithPredicate (predicate: Var -> bool) (expr: Expr) : Set<Var> =
        expr.GetFreeVars() |> Seq.filter predicate |> Set.ofSeq

    let isFunction (var: Var) = FSharpType.IsFunction var.Type

    let collectFreeVars: Expr -> Set<Var> =
        collectFreeVarsWithPredicate (not << isFunction)

    let collectFreeFunctionVars: Expr -> Set<Var> =
        collectFreeVarsWithPredicate isFunction

    let getLocalVars expr =
        let rec get acc =
            function
            // TODO(Note: precomputation in specificCall, make static?)
            | Patterns.Let(var, DerivedPatterns.SpecificCall <@ local @> _, body)
            | Patterns.Let(var, DerivedPatterns.SpecificCall <@ localArray @> _, body) -> get (var :: acc) body
            | ExprShape.ShapeVar _ -> acc
            | ExprShape.ShapeLambda(_, lambda) -> get acc lambda
            | ExprShape.ShapeCombination(_, exp) -> List.collect (get acc) exp

        get [] expr

    let createRefVar (var: Var) =
        let refName = var.Name + "Ref"
        let refType = typedefof<ref<_>>.MakeGenericType var.Type

        Var(refName, refType, false)

    // TODO(make static)
    let createRefCall (value: Expr) =
        match <@@ ref () @@> with
        | Patterns.Call(obj, methodInfo, _) ->
            let newMethodInfo =
                methodInfo.GetGenericMethodDefinition().MakeGenericMethod([| value.Type |])

            match obj with
            | Some obj -> Expr.Call(obj, newMethodInfo, [ value ])
            | None -> Expr.Call(newMethodInfo, [ value ])
        | _ -> failwithf "createRefCall: ref () is not more a Call expression"

    let createDereferenceCall (reference: Expr) =
        match <@@ !(ref ()) @@> with
        | Patterns.Call(None, methodInfo, _) ->
            let tp = reference.Type.GenericTypeArguments.[0]

            let newMethodInfo =
                methodInfo.GetGenericMethodDefinition().MakeGenericMethod([| tp |])

            Expr.Call(newMethodInfo, [ reference ])
        | _ -> failwithf "createDereferenceCall: ! is not more a Call expression"

    let createReferenceSetCall (reference: Expr) (value: Expr) =
        match <@@ ref () := () @@> with
        | Patterns.Call(None, methodInfo, _) ->
            let tp = reference.Type.GenericTypeArguments.[0]
            let newMethodInfo = methodInfo.GetGenericMethodDefinition().MakeGenericMethod(tp)

            Expr.Call(newMethodInfo, [ reference; value ])
        | _ -> failwithf "createReferenceSetCall: (:=) is not more a Call expression"

    let isGlobal (var: Var) =
        var.Type.Name.ToLower().StartsWith ClArray_
        || var.Type.Name.ToLower().StartsWith ClCell_
