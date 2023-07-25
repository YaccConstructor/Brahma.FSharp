namespace Brahma.FSharp.OpenCL.Translator.QuotationTransformers

open FSharp.Reflection
open FSharp.Quotations
open Brahma.FSharp
open Brahma.FSharp.OpenCL.Translator

module Utils =
    let rec getFunctionArgTypes (funType: System.Type) =
        let (argType, retType) = FSharpType.GetFunctionElements(funType)
        match retType with
        | _ when FSharpType.IsFunction retType ->
            argType :: getFunctionArgTypes retType
        | _ ->  [argType]

    let makeFunctionType (retType: System.Type) (argTypes: List<System.Type>) =
        List.foldBack (fun tp acc ->  FSharpType.MakeFunctionType(tp, acc)) argTypes retType

    let makeLambdaType types =
        List.reduceBack (fun domain range -> FSharpType.MakeFunctionType(domain, range)) types

    let rec makeLambdaExpr (args: Var list) (body: Expr) =
        let mkLambda var expr = Expr.Lambda(var, expr)
        List.foldBack mkLambda args body

    let rec makeApplicationExpr (head: Expr) (exprs: Expr list) =
        let mkApplication l r = Expr.Application(l, r)
        List.fold mkApplication head exprs

    let rec extractLambdaArguments (expr: Expr) =
        match expr with
        | Patterns.Lambda (var, body) ->
            let vars, body' = extractLambdaArguments body
            var :: vars, body'
        | _ -> [], expr

    let rec collectLambdaArguments (expr: Expr) : List<Var> =
        match expr with
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

        | ExprShape.ShapeVar var -> []
        | ExprShape.ShapeLambda (var, lambda) ->
            collectLocalVars lambda
        | ExprShape.ShapeCombination (_, exprs) ->
            exprs
            |> List.collect collectLocalVars

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
