namespace Brahma.FSharp.OpenCL.Translator.QuotationsTransformer

open FSharp.Quotations
open FSharp.Reflection
open Microsoft.FSharp.Core.LanguagePrimitives

[<AutoOpen>]
module LetVarAbstracter =
    let rec isPrimitiveExpression (expr: Expr) =
        match expr with
        | Patterns.Value _
        | Patterns.ValueWithName _
        | Patterns.Var _ -> true
        | Patterns.Call (_, _, args) -> List.forall isPrimitiveExpression args
        // | DerivedPatterns.Applications (func, applicationArgs) ->
        //     // applicationArgs
        //     // |> List.collect id
        //     // |> List.forall isPrimitiveExpression
        //     // &&
        //     // isPrimitiveExpression func
        //     true
        | Patterns.FieldGet (instance, _) ->
            instance
            |> Option.map isPrimitiveExpression
            |> Option.defaultValue true
        | Patterns.PropertyGet (instance, _, args) ->
            let isPrimitiveInstance =
                instance
                |> Option.map isPrimitiveExpression
                |> Option.defaultValue true

            let isPrimitiveArgs = List.forall isPrimitiveExpression args
            isPrimitiveInstance && isPrimitiveArgs
        // | DerivedPatterns.SpecificCall <@ IntrinsicFunctions.GetArray @> (_, _, args) ->
        //     List.forall isPrimitiveExpression args
        | Patterns.NewUnionCase _ -> true
        | _ -> false

    // NOTE не оч понимаю, заечм это изначально нужно, но это полезно,
    // когда тело ффункции зависит от конкретного применения
    // let x = expr -> let x = let unit () = expr in unit ()
    let rec varDefsToLambda (expr: Expr) =
        match expr with
        | Patterns.LetVar (var, body, inExpr) ->
            // match body with
            // | Patterns.Let (var, body, inExpr) ->

            if isPrimitiveExpression body then
                Expr.Let(var, body, varDefsToLambda inExpr)
            else
                let fType = FSharpType.MakeFunctionType(typeof<unit>, var.Type)
                let fVar = Var(var.Name + "UnitFunc", fType)

                Expr.Let(
                    var,
                    Expr.Let(
                        fVar,
                        Expr.Lambda(Var("unitVar", typeof<unit>), varDefsToLambda body),
                        Expr.Application(Expr.Var fVar, Expr.Value((), typeof<unit>))
                    ),
                    varDefsToLambda inExpr
                )

        | ExprShape.ShapeVar _ -> expr
        | ExprShape.ShapeLambda (var, body) -> Expr.Lambda(var, varDefsToLambda body)
        | ExprShape.ShapeCombination (shapeComboObject, exprList) ->
            let exprList' = List.map varDefsToLambda exprList
            ExprShape.RebuildShapeCombination(shapeComboObject, exprList')
