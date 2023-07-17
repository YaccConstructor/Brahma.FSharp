namespace Brahma.FSharp.OpenCL.Translator.QuotationTransformers

open FSharp.Quotations
open FSharp.Reflection

module Variables =
    // TODO need way to identify expression vs statements (now it is very primitive)
    let rec private isPrimitiveExpression =
        function
        | Patterns.Value _
        | Patterns.ValueWithName _
        | Patterns.DefaultValue _
        | Patterns.Var _ -> true
        | Patterns.Call(_, _, args) -> List.forall isPrimitiveExpression args
        | Patterns.FieldGet(instance, _) -> instance |> Option.map isPrimitiveExpression |> Option.defaultValue true
        | Patterns.PropertyGet(instance, _, args) ->
            let isPrimitiveInstance =
                instance |> Option.map isPrimitiveExpression |> Option.defaultValue true

            let isPrimitiveArgs = List.forall isPrimitiveExpression args

            isPrimitiveInstance && isPrimitiveArgs
        | Patterns.NewUnionCase _ -> true
        | _ -> false

    // create: let fVal () = expr in unit ()
    let private createApplication fVar body =
        Expr.Let(
            fVar,
            Expr.Lambda(Var("unitVar", typeof<unit>), body),
            Expr.Application(Expr.Var fVar, Expr.Value((), typeof<unit>))
        )

    // let x = expr -> let x = let unit () = expr in unit ()
    let rec defsToLambda =
        function
        | Patterns.LetVar(var, body, inExpr) ->
            if isPrimitiveExpression body then
                Expr.Let(var, body, defsToLambda inExpr)
            else
                let funVal =
                    let fType = FSharpType.MakeFunctionType(typeof<unit>, var.Type)

                    Var(var.Name + "UnitFunc", fType)

                let body = defsToLambda body
                let letEvalAndApplication = createApplication funVal body

                let newInExpr = defsToLambda inExpr

                Expr.Let(var, letEvalAndApplication, newInExpr)
        | Patterns.PropertySet(Some o, prop, indices, value) ->
            if isPrimitiveExpression value then
                Expr.PropertySet(o, prop, value, indices)
            else
                let fType = FSharpType.MakeFunctionType(typeof<unit>, prop.PropertyType)
                let fVal = Var(prop.Name + "UnitFunc", fType)

                let body = defsToLambda value
                let letEvalAndApplication = createApplication fVal body

                Expr.PropertySet(o, prop, letEvalAndApplication, indices)
        | ExprShape.ShapeVar _ as expr -> expr
        | ExprShape.ShapeLambda(var, body) -> Expr.Lambda(var, defsToLambda body)
        | ExprShape.ShapeCombination(shapeComboObject, exprList) ->
            let exprList' = List.map defsToLambda exprList

            ExprShape.RebuildShapeCombination(shapeComboObject, exprList')
