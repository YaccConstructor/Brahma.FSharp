namespace Brahma.FSharp.OpenCL.Translator.QuotationTransformers

open FSharp.Reflection
open FSharp.Quotations
open Brahma.FSharp
open Brahma.FSharp.OpenCL.Translator

module Utils =
    let rec getFunctionArgTypes (funType: System.Type) =
        let argType, retType = FSharpType.GetFunctionElements(funType)
        match retType with
        | _ when FSharpType.IsFunction retType ->
            argType :: getFunctionArgTypes retType
        | _ ->  [argType]

    let makeFunctionType (retType: System.Type) (argTypes: List<System.Type>) =
        List.foldBack (fun tp acc ->  FSharpType.MakeFunctionType(tp, acc)) argTypes retType

    let makeLambdaType types =
        List.reduceBack (fun domain range -> FSharpType.MakeFunctionType(domain, range)) types

    let makeLambdaExpr (args: Var list) (body: Expr) =
        List.foldBack (fun var expr -> Expr.Lambda(var, expr)) args body

    let makeApplicationExpr (head: Expr) (expressions: Expr list) =
        List.fold (fun l r -> Expr.Application(l, r)) head expressions

    // TODO tail recursion
    let rec extractLambdaArguments = function
        | Patterns.Lambda (var, body) ->
            let vars, body' = extractLambdaArguments body
            var :: vars, body'
        | expr -> [], expr

    let rec collectLambdaArguments = function
        | ExprShape.ShapeLambda (var, body) ->
            var :: collectLambdaArguments body
        | _ -> []

    // Это из замыкания переменные?
    /// Collect free variables of expression that satisfies predicate.
    let rec collectFreeVarsWithPredicate (predicate: Var -> bool) (expr: Expr) : Set<Var> =
        match expr with
        | Patterns.Let (var, expr, inExpr) ->
            Set.union
            <| collectFreeVarsWithPredicate predicate expr
            <| Set.remove var (collectFreeVarsWithPredicate predicate inExpr)

        | ExprShape.ShapeVar var ->
            if predicate var then Set.singleton var else Set.empty

        | ExprShape.ShapeLambda (var, expr) ->
            expr
            |> collectFreeVarsWithPredicate predicate
            |> Set.remove var

        | ExprShape.ShapeCombination (_, exprs) ->
            exprs
            |> List.map (collectFreeVarsWithPredicate predicate)
            |> Set.unionMany

    let isFunction (var: Var) =
        FSharpType.IsFunction var.Type

    let collectFreeVars : Expr -> Set<Var> =
        collectFreeVarsWithPredicate (not << isFunction)

    let collectFreeFunctionVars : Expr -> Set<Var> =
        collectFreeVarsWithPredicate isFunction

    let rec collectLocalVars (expr: Expr) : Var list =
        match expr with
        | Patterns.Let (variable, DerivedPatterns.SpecificCall <@ local @> (_, _, _), cont)
        | Patterns.Let (variable, DerivedPatterns.SpecificCall <@ localArray @> (_, _, _), cont) ->
            variable :: collectLocalVars cont
        | ExprShape.ShapeVar _ -> []
        | ExprShape.ShapeLambda (_, lambda) ->
            collectLocalVars lambda
        | ExprShape.ShapeCombination (_, expressions) ->
            List.collect collectLocalVars expressions

    let isTypeOf<'tp> (var: Var) =
        var.Type = typeof<'tp>

    let createRefCall (value: Expr) =
        match <@@ ref () @@> with
        | Patterns.Call(obj, methodInfo, _) ->
            let newMethodInfo = methodInfo.GetGenericMethodDefinition().MakeGenericMethod([|value.Type|])
            match obj with
            | Some obj -> Expr.Call(obj, newMethodInfo, [value])
            | None -> Expr.Call(newMethodInfo, [value])
        | _ -> failwithf "createRefCall: ref () is not more a Call expression"

    let createDereferenceCall (reference: Expr) =
        match <@@ ! (ref ()) @@> with
        | Patterns.Call(None, methodInfo, _) ->
            let tp = reference.Type.GenericTypeArguments.[0]
            let newMethodInfo = methodInfo.GetGenericMethodDefinition().MakeGenericMethod([|tp|])
            Expr.Call (newMethodInfo, [reference])
        | _ -> failwithf "createDereferenceCall: ! is not more a Call expression"

    let createReferenceSetCall (reference: Expr) (value: Expr) =
        match <@@ ref () := () @@> with
        | Patterns.Call (None, methodInfo, _) ->
            let tp = reference.Type.GenericTypeArguments.[0]
            let newMethodInfo = methodInfo.GetGenericMethodDefinition().MakeGenericMethod(tp)
            Expr.Call (newMethodInfo, [reference; value])
        | _ -> failwithf "createReferenceSetCall: (:=) is not more a Call expression"

    let isGlobal (var: Var) =
        var.Type.Name.ToLower().StartsWith ClArray_ ||
        var.Type.Name.ToLower().StartsWith ClCell_
