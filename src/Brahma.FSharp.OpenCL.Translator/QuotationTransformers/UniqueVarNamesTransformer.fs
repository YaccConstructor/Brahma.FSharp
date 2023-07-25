namespace Brahma.FSharp.OpenCL.Translator.QuotationTransformers

open System.Collections.Generic
open FSharp.Quotations

type RenamingContext() =
    let varMapper = Dictionary<Var, Var>()
    let totalNames = HashSet<string>()
    let mutable counter = 0

    let makeUniqueVarName (varName: string) =
        if totalNames.Contains varName then
            counter <- counter + 1
            sprintf "%s%d" varName counter
        else
            varName

    member this.Add(var: Var) =
        if not <| varMapper.ContainsKey var then
            let newName = makeUniqueVarName var.Name
            let newVar = Var(newName, var.Type, var.IsMutable)
            varMapper.Add(var, newVar)
            totalNames.Add newName |> ignore

        varMapper.[var]

    member this.Mapper = varMapper

[<AutoOpen>]
module UniqueVarRenamer =
    let rec private makeVarNamesUniqueImpl (ctx: RenamingContext) (expr: Expr) =
        match expr with
        | ExprShape.ShapeVar var ->
            let newVar = ctx.Add var
            Expr.Var(newVar)
        | ExprShape.ShapeLambda (var, body) ->
            let newVar = ctx.Add var
            Expr.Lambda(newVar, makeVarNamesUniqueImpl ctx body)
        | ExprShape.ShapeCombination (shapeComboObj, exprList) ->
            let exprList' = List.map (makeVarNamesUniqueImpl ctx) exprList
            ExprShape.RebuildShapeCombination (shapeComboObj, exprList')

    let makeVarNameUnique (expr: Expr) =
        makeVarNamesUniqueImpl <| RenamingContext () <| expr
