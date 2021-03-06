module Brahma.FSharp.OpenCL.QuotationsTransformer.Utils.Common

open FSharp.Reflection
open FSharp.Quotations

let rec getFunctionArgTypes (funType: System.Type) =
    let argType, retType = FSharpType.GetFunctionElements(funType)
    match retType with
    | _ when FSharpType.IsFunction retType ->
        argType :: getFunctionArgTypes retType
    | _ ->  [argType]

let makeFunctionType (retType: System.Type) (argTypes: List<System.Type>) =
    List.foldBack (fun tp acc ->  FSharpType.MakeFunctionType (tp, acc)) argTypes retType

let rec makeLambdaExpr (args: list<Var>) (body: Expr) =
    let mkLambda var expr = Expr.Lambda(var, expr)
    List.foldBack mkLambda args body

let rec makeApplicationExpr (head: Expr) (exprs: list<Expr>) =
    let mkApplication l r = Expr.Application(l, r)
    List.fold mkApplication head exprs

let rec extractLambdaArguments (expr: Expr): list<Var> * Expr =
    match expr with
    | Patterns.Lambda(var, body) ->
        let vars, body' = extractLambdaArguments body
        var :: vars, body'
    | _ -> [], expr

let isFunction (var: Var) =
    FSharpType.IsFunction var.Type

/// Collect free variables of expression that satisfies predicate.
let rec collectFreeVarsWithPredicate (predicate: Var -> bool) (expr: Expr): Set<Var> =
    match expr with
    | Patterns.Let (var, expr, inExpr) ->
        Set.union <|
            collectFreeVarsWithPredicate predicate expr <|
            Set.remove var (collectFreeVarsWithPredicate predicate inExpr)

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

let collectFreeVars: Expr -> Set<Var> =
    collectFreeVarsWithPredicate (not << isFunction)

let collectFreeFunctionVars: Expr -> Set<Var> =
    collectFreeVarsWithPredicate isFunction

let rec collectLambdaArguments (expr: Expr) : List<Var> =
    match expr with
    | ExprShape.ShapeLambda (var, body) ->
        var :: collectLambdaArguments body
    | _ -> []

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
