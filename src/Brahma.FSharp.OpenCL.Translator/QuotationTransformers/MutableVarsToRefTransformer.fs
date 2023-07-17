namespace Brahma.FSharp.OpenCL.Translator.QuotationTransformers

open FSharp.Quotations

[<AutoOpen>]
module MutableVarsToRefTransformer =
    let private isMutableVar (var: Var) =
        var.IsMutable && not (Utils.isFunction var)

    let rec collectMutableVarsInClosure = function
        | Patterns.LetFunc(_, body, inExpr) ->
            let mutableFreeVars = body |> Utils.collectFreeVarsWithPredicate isMutableVar

            Set.unionMany
                [ mutableFreeVars
                  collectMutableVarsInClosure body
                  collectMutableVarsInClosure inExpr ]
        | ExprShape.ShapeLambda(_, body) -> collectMutableVarsInClosure body
        | ExprShape.ShapeVar _ -> Set.empty
        | ExprShape.ShapeCombination(_, exprList) -> exprList |> List.map collectMutableVarsInClosure |> Set.unionMany

    let rec varsToRefsWithPredicateImpl (refMap: Map<Var, Expr>) (predicate: Var -> bool) (expr: Expr) =
        match expr with
        | Patterns.LetVar(var, body, inExpr) ->
            if predicate var then
                let refName = var.Name + "Ref"
                let refType = typedefof<ref<_>>.MakeGenericType (var.Type)
                let refVar = Var(refName, refType, false)

                let newRefMap = refMap.Add(var, Expr.Var refVar)

                Expr.Let(
                    var,
                    varsToRefsWithPredicateImpl refMap predicate body,
                    Expr.Let(
                        refVar,
                        Utils.createRefCall <| Expr.Var var,
                        varsToRefsWithPredicateImpl newRefMap predicate inExpr
                    )
                )
            else
                Expr.Let(
                    var,
                    varsToRefsWithPredicateImpl refMap predicate body,
                    varsToRefsWithPredicateImpl refMap predicate inExpr
                )

        | Patterns.VarSet(var, valueExpr) ->
            match refMap.TryFind var with
            | Some refExpr ->
                Utils.createReferenceSetCall refExpr
                <| varsToRefsWithPredicateImpl refMap predicate valueExpr
            | None -> expr

        | ExprShape.ShapeVar var ->
            match refMap.TryFind var with
            | Some refExpr -> Utils.createDereferenceCall refExpr
            | None -> expr
        | ExprShape.ShapeLambda(var, body) -> Expr.Lambda(var, varsToRefsWithPredicateImpl refMap predicate body)
        | ExprShape.ShapeCombination(shapeComboObject, exprList) ->
            let exprList' = List.map (varsToRefsWithPredicateImpl refMap predicate) exprList
            ExprShape.RebuildShapeCombination(shapeComboObject, exprList')

    let varsToRefsWithPredicate (predicate: Var -> bool) (expr: Expr) =
        varsToRefsWithPredicateImpl Map.empty predicate expr

    let transformMutableVarsToRef (expr: Expr) =
        let mutableVarsInClosure = collectMutableVarsInClosure expr
        varsToRefsWithPredicate mutableVarsInClosure.Contains expr
