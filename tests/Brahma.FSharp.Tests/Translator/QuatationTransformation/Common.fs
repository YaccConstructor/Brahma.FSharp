module Brahma.FSharp.Tests.Translator.QuatationTransformation.Common

open Expecto
open FSharp.Quotations
open Brahma.FSharp.OpenCL.Translator

[<AutoOpen>]
module Helpers =
    let equalsMessage = "Values should be the same."

    let rec renameUnitVar (expr: Expr) =
        let replaceUnitVar (var: Var) =
            if var.Type = typeof<unit> then
                Var("unitVar", var.Type, var.IsMutable)
            else
                var

        match expr with
        | ExprShape.ShapeVar var -> Expr.Var(replaceUnitVar var)
        | ExprShape.ShapeLambda(var, body) -> Expr.Lambda(replaceUnitVar var, renameUnitVar body)
        | ExprShape.ShapeCombination(shapeComboObj, exprList) ->
            ExprShape.RebuildShapeCombination(shapeComboObj, List.map renameUnitVar exprList)

    let openclTransformQuotation (translator: FSQuotationToOpenCLTranslator) (expr: Expr) =
        translator.TransformQuotation expr

    let assertExprEqual (actual: Expr) (expected: Expr) (msg: string) =
        let actual' = renameUnitVar actual
        let expected' = renameUnitVar expected

        Expect.sequenceEqual <| actual'.ToString() <| expected'.ToString() <| msg

    let assertMethodEqual (actual: Var * Expr) (expected: Var * Expr) =
        Expect.equal (fst actual).Name (fst expected).Name "Method names should be equal"

        assertExprEqual (snd actual) (snd expected)
        <| $"Method bodies of %s{(fst actual).Name} is not equal"
