namespace Brahma.FSharp.OpenCL.Translator.QuotationTransformers

open FSharp.Quotations
open Brahma.FSharp.OpenCL.Translator

type WorkSizeQual =
    | GlobalWS
    | LocalWS

module GettingWorkSizeTransformer =
    let inline (|Name|_|) (str: string) x =
        match (^a : (member Name : string) x) with
        | name when name = str -> Some x
        | _ -> None

    let inline (|TypeName|_|) (str: string) x =
        match (^a : (member Type : System.Type) x) with
        | type' when type'.Name.ToLowerInvariant().Contains str -> Some x
        | _ -> None

    let inline (|WorkSize|_|) x =
        match x with
        | Name "GlobalWorkSize" x -> Some (x, GlobalWS)
        | Name "LocalWorkSize" x -> Some (x, LocalWS)
        | _ -> None

    let rec go (expr: Expr) =
        match expr with
        | Patterns.Let
            (
                var,
                Patterns.PropertyGet (Some (Patterns.Var (TypeName Range1D_ _)), WorkSize (_, q), _),
                inExpr
            ) ->
            Expr.Let(
                var,
                (match q with | GlobalWS -> <@@ Anchors._globalSize0 @@> | LocalWS -> <@@ Anchors._localSize0 @@>),
                go inExpr
            )

        | Patterns.LetVar
            (
                Name "patternInput" _,
                Patterns.PropertyGet (Some (Patterns.Var (TypeName Range2D_ _)), WorkSize (_, q), _),
                Patterns.Let (
                    varY,
                    Patterns.TupleGet (Patterns.Var (Name "patternInput" _), 1),
                    Patterns.Let (
                        varX,
                        Patterns.TupleGet (Patterns.Var (Name "patternInput" _), 0),
                        inExpr
                    )
                )
            ) ->
            Expr.Let(
                varX,
                (match q with | GlobalWS -> <@@ Anchors._globalSize0 @@> | LocalWS -> <@@ Anchors._localSize0 @@>),
                Expr.Let(
                    varY,
                    (match q with | GlobalWS -> <@@ Anchors._globalSize1 @@> | LocalWS -> <@@ Anchors._localSize1 @@>),
                    go inExpr
                )
            )

        | Patterns.LetVar
            (
                Name "patternInput" _,
                Patterns.PropertyGet (Some (Patterns.Var (TypeName Range3D_ _)), WorkSize (_, q), _),
                Patterns.Let (
                    varZ,
                    Patterns.TupleGet (Patterns.Var (Name "patternInput" _), 2),
                    Patterns.Let (
                        varY,
                        Patterns.TupleGet (Patterns.Var (Name "patternInput" _), 1),
                        Patterns.Let (
                            varX,
                            Patterns.TupleGet (Patterns.Var (Name "patternInput" _), 0),
                            inExpr
                        )
                    )
                )
            ) ->
            Expr.Let(
                varX,
                (match q with | GlobalWS -> <@@ Anchors._globalSize0 @@> | LocalWS -> <@@ Anchors._localSize0 @@>),
                Expr.Let(
                    varY,
                    (match q with | GlobalWS -> <@@ Anchors._globalSize1 @@> | LocalWS -> <@@ Anchors._localSize1 @@>),
                    Expr.Let(
                        varZ,
                        (match q with | GlobalWS -> <@@ Anchors._globalSize2 @@> | LocalWS -> <@@ Anchors._localSize2 @@>),
                        go inExpr
                    )
                )
            )

        | ExprShape.ShapeVar var -> Expr.Var var
        | ExprShape.ShapeLambda (var, lambda) ->
            Expr.Lambda(var, go lambda)
        | ExprShape.ShapeCombination (combo, exprs) ->
            ExprShape.RebuildShapeCombination(combo, List.map go exprs)

    let __ (expr: Expr) =
        go expr
