namespace Brahma.FSharp.OpenCL.Translator.QuotationTransformers

open FSharp.Quotations

module VarToRef =
    let private isMutableVar (var: Var) = var.IsMutable && not (Utils.isFunction var)

    let rec private collectMutableVarsInClosure =
        function
        | Patterns.LetFunc(_, body, inExpr) ->
            let mutableFreeVars = body.GetFreeVars() |> Seq.filter isMutableVar |> Set.ofSeq

            [
                mutableFreeVars
                collectMutableVarsInClosure body
                collectMutableVarsInClosure inExpr
            ]
            |> Set.unionMany
        | ExprShape.ShapeLambda(_, body) -> collectMutableVarsInClosure body
        | ExprShape.ShapeVar _ -> Set.empty
        | ExprShape.ShapeCombination(_, exprList) -> exprList |> List.map collectMutableVarsInClosure |> Set.unionMany

    let private varsToRefsWithPredicate (predicate: Var -> bool) (expr: Expr) =
        let rec parse (refMap: Map<Var, Expr>) =
            function
            | Patterns.LetVar(var, body, inExpr) ->
                if predicate var then
                    // create refVar, typeof<refVar> = ref<typeof<var>>
                    let refVar = Utils.createRefVar var
                    // map var to refVar usage
                    let newRefMap = refMap.Add(var, Expr.Var refVar)
                    let refInExpr = parse newRefMap inExpr

                    // <@ ref var @>
                    let refCall = Utils.createRefCall <| Expr.Var var

                    // <@ let refVar = ref var in refInExpr @>
                    let newLetInExpr = Expr.Let(refVar, refCall, refInExpr)

                    let newBody = parse refMap body

                    // let var = newBody in
                    // let refVar = ref (var) in
                    // refInExpr
                    Expr.Let(var, newBody, newLetInExpr)
                else
                    let body = parse refMap body
                    let inExp = parse refMap inExpr

                    Expr.Let(var, body, inExp)
            | Patterns.VarSet(var, valueExpr) as sourceExpr ->
                refMap.TryFind var
                |> Option.map (fun refExpr ->
                    let expr = parse refMap valueExpr
                    Utils.createReferenceSetCall refExpr expr
                )
                |> Option.defaultValue sourceExpr
            | ExprShape.ShapeVar var as sourceExpr ->
                refMap.TryFind var
                |> Option.map Utils.createDereferenceCall
                |> Option.defaultValue sourceExpr
            | ExprShape.ShapeLambda(var, body) ->
                let newBody = parse refMap body

                Expr.Lambda(var, newBody)
            | ExprShape.ShapeCombination(shapeComboObject, exprList) ->
                let exprList' = List.map (parse refMap) exprList

                ExprShape.RebuildShapeCombination(shapeComboObject, exprList')

        parse Map.empty expr

    let transform (expr: Expr) =
        let mutableVarsInClosure = collectMutableVarsInClosure expr

        varsToRefsWithPredicate mutableVarsInClosure.Contains expr
