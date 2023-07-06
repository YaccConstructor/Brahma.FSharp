namespace Brahma.FSharp.OpenCL.Translator.QuotationTransformers

open Brahma.FSharp.OpenCL.Translator
open FSharp.Quotations

type Context =
    private
        { FreeVariables: Map<Var, List<Var>>
          Substitution: Map<Var, Expr> }

module Context =
    /// head: t, args: [x1: t1; x2: t2; x3: t3]
    /// result: (newHead: t1 -> t2 -> t3 -> t) x1 x2 x3
    let makeApplication (head: Var) (args: List<Var>) =
        let argTypes = List.map (fun (x: Var) -> x.Type) args
        let newHeadType = Utils.makeFunctionType head.Type argTypes

        let newHead = Var(head.Name, newHeadType, head.IsMutable)

        let application =
            args
            |> List.map Expr.Var
            |> List.fold (fun acc expr -> Expr.Application(acc, expr)) (Expr.Var newHead)

        application, newHead

    let empty =
        { FreeVariables = Map.empty
          Substitution = Map.empty }

    let setFunctionFreeVariables (oldFuncVar: Var) (extendedParams: List<Var>) (ctx: Context) =
        { FreeVariables = ctx.FreeVariables.Add(oldFuncVar, extendedParams)
          Substitution = ctx.Substitution }

    let setFunctionSubstitution (oldFuncVar: Var) (substitution: Expr) (ctx: Context) =
        { FreeVariables = ctx.FreeVariables
          Substitution = ctx.Substitution.Add(oldFuncVar, substitution) }

    let getFunctionFreeVariables (oldFuncVar: Var) (ctx: Context) = ctx.FreeVariables.TryFind oldFuncVar

    let getFunctionSubstitution (oldFuncVar: Var) (ctx: Context) = ctx.Substitution.TryFind oldFuncVar

module VoidArgumentsCleanUp =
    let private isConsistOfVoidVarOnly (args: list<Var>) =
        args.Length = 1 && args.Head.Type = typeof<unit>

    let private isConsistOfVoidExprOnly (args: list<Expr>) =
        args.Length = 1 && args.Head.Type = typeof<unit>

    let rec private cleanUpVoidArgumentsImpl (subst: Map<Var, Var>) (expr: Expr) =
        match expr with
        | Patterns.LetFuncUncurry(var, args, body, inExpr) ->
            let args' =
                if isConsistOfVoidVarOnly args then
                    args
                else
                    List.filter (not << Utils.isTypeOf<unit>) args

            let newFuncVarType =
                Utils.makeFunctionType body.Type <| List.map (fun (var: Var) -> var.Type) args'

            let newFuncVar = Var(var.Name, newFuncVarType, var.IsMutable)
            let body' = cleanUpVoidArgumentsImpl subst body

            let subst' = subst.Add(var, newFuncVar)
            let inExpr' = cleanUpVoidArgumentsImpl subst' inExpr
            Expr.Let(newFuncVar, Utils.makeLambdaExpr args' body', inExpr')

        | Patterns.ApplicationUncurry(head, exprs) ->
            match head with
            | Patterns.Var var ->
                match subst.TryFind var with
                | Some var' ->
                    let exprs' =
                        if isConsistOfVoidExprOnly exprs then
                            exprs
                        else
                            List.filter (fun (exp: Expr) -> exp.Type <> typeof<unit>) exprs

                    Utils.makeApplicationExpr
                    <| Expr.Var var'
                    <| List.map (cleanUpVoidArgumentsImpl subst) exprs'

                | _ -> expr
            | _ -> expr
        | ExprShape.ShapeLambda(var, body) -> Expr.Lambda(var, cleanUpVoidArgumentsImpl subst body)
        | ExprShape.ShapeVar var ->
            match subst.TryFind var with
            | Some _ ->
                // TODO: check it in another step.
                failwithf "First-Order functions (just like curring) is not supported."
            | None -> expr
        | ExprShape.ShapeCombination(shapeComboObject, exprList) ->
            let exprList' = List.map <| cleanUpVoidArgumentsImpl subst <| exprList
            ExprShape.RebuildShapeCombination(shapeComboObject, exprList')

    let cleanUpVoidArguments (expr: Expr) = cleanUpVoidArgumentsImpl Map.empty expr

[<AutoOpen>]
module LambdaLifting =
    let rec parameterLiftExprImpl (ctx: Context) (expr: Expr) =
        match expr with
        | Patterns.LetVar(v, definition, inExpr) ->
            Expr.Let(v, parameterLiftExprImpl ctx definition, parameterLiftExprImpl ctx inExpr)

        | Patterns.LetFunc(f, definition, inExpr) ->
            let localFreeVars = Utils.collectFreeVars definition
            let freeFunctionVars = Utils.collectFreeFunctionVars definition

            let getSetFreeVars (fVar: Var) =
                Context.getFunctionFreeVariables fVar ctx
                |> Option.defaultValue List.empty
                |> Set.ofList

            let extendedFreeVars = freeFunctionVars |> Set.map getSetFreeVars |> Set.unionMany

            let freeVars = Set.union localFreeVars extendedFreeVars |> Set.toList

            let (substitution, newFuncVar) = Context.makeApplication f freeVars
            let newDefinition = parameterLiftExprImpl ctx definition

            let extendedCtx =
                ctx
                |> Context.setFunctionFreeVariables f freeVars
                |> Context.setFunctionSubstitution f substitution

            Expr.Let(
                newFuncVar,
                List.foldBack (fun arg body -> Expr.Lambda(arg, body)) freeVars newDefinition,
                inExpr |> parameterLiftExprImpl extendedCtx
            )

        | ExprShape.ShapeLambda(x, body) -> Expr.Lambda(x, parameterLiftExprImpl ctx body)

        | ExprShape.ShapeVar var ->
            match Context.getFunctionSubstitution var ctx with
            | Some subst -> subst
            | None -> expr

        | ExprShape.ShapeCombination(shapeComboObject, exprList) ->
            ExprShape.RebuildShapeCombination(shapeComboObject, List.map (parameterLiftExprImpl ctx) exprList)

    let parameterLiftExpr = parameterLiftExprImpl Context.empty

    let rec blockFloating (expr: Expr) =
        match expr with
        | Patterns.LetFunc(var, body, inExpr) ->
            let (body', bodyMethods) = blockFloating body
            let (inExpr', inExprMethods) = blockFloating inExpr
            inExpr', bodyMethods @ [ (var, body') ] @ inExprMethods

        | ExprShape.ShapeLambda(var, body) ->
            let (body', methods) = blockFloating body
            Expr.Lambda(var, body'), methods

        | ExprShape.ShapeVar var -> Expr.Var(var), List.empty

        | ExprShape.ShapeCombination(shapeComboObject, exprList) ->
            let (exprList', methods) = exprList |> List.map blockFloating |> List.unzip
            ExprShape.RebuildShapeCombination(shapeComboObject, exprList'), List.concat methods

    let lambdaLifting (expr: Expr) =
        expr
        |> parameterLiftExpr
        |> VoidArgumentsCleanUp.cleanUpVoidArguments
        |> blockFloating
