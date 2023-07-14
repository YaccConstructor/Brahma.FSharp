namespace Brahma.FSharp.OpenCL.Translator.QuotationTransformers

open FSharp.Quotations
open Brahma.FSharp.OpenCL.Translator

module WorkSize =
    type private WorkSizeQual =
        | GlobalWS
        | LocalWS

    let inline private (|Name|_|) (str: string) x =
        match (^a: (member Name: string) x) with
        | name when name = str -> Some()
        | _ -> None

    let inline private (|WorkSize|_|) x =
        match x with
        | Name "GlobalWorkSize" -> Some(GlobalWS)
        | Name "LocalWorkSize" -> Some(LocalWS)
        | _ -> None

    let inline private (|TypeName|_|) (str: string) x =
        match (^a: (member Type: System.Type) x) with
        | type' when type'.Name.ToLowerInvariant().Contains str -> Some()
        | _ -> None

    let inline private (|Qualifier|_|) name =
        function
        | Patterns.PropertyGet(Some(Patterns.Var(TypeName name)), WorkSize(qualifier), _) -> Some qualifier
        | _ -> None

    let inline private (|Equal|_|) str =
        function
        | line when line = str -> Some()
        | _ -> None

    let private (|ReturnSome|_|) =
        function
        | x -> Some x

    let inline private (|CoordinateBind|_|) number (|InPatter|_|) =
        function
        | Patterns.Let(var, Patterns.TupleGet(Patterns.Var(Name "patternInput"), Equal number), InPatter inExp) ->
            Some(var, inExp)
        | _ -> None

    let inline private (|Zero|_|) exp =
        (|CoordinateBind|_|) 0 (|ReturnSome|_|) exp

    let inline private (|First|_|) exp = (|CoordinateBind|_|) 1 (|Zero|_|) exp

    let inline private (|Second|_|) exp = (|CoordinateBind|_|) 2 (|First|_|) exp

    let private globalSize0 =
        function
        | GlobalWS -> <@@ Anchors._globalSize0 @@>
        | LocalWS -> <@@ Anchors._localSize0 @@>

    let private globalSize1 =
        function
        | GlobalWS -> <@@ Anchors._globalSize1 @@>
        | LocalWS -> <@@ Anchors._localSize1 @@>

    let private globalSize2 =
        function
        | GlobalWS -> <@@ Anchors._globalSize2 @@>
        | LocalWS -> <@@ Anchors._localSize2 @@>

    let rec go =
        function
        | Patterns.Let(var, Qualifier Range1D_ qualifier, inExpr) -> Expr.Let(var, (globalSize0 qualifier), go inExpr)
        | Patterns.LetVar(Name "patternInput", Qualifier Range2D_ qualifier, First(varY, (varX, inExp))) ->
            let inExp = Expr.Let(varY, (globalSize1 qualifier), go inExp)

            Expr.Let(varX, (globalSize0 qualifier), inExp)
        | Patterns.LetVar(Name "patternInput", Qualifier Range2D_ qualifier, Second(varZ, (varY, (varX, inExp)))) ->
            let inExp = Expr.Let(varZ, (globalSize2 qualifier), go inExp)
            let inExp = Expr.Let(varY, (globalSize1 qualifier), inExp)

            Expr.Let(varX, (globalSize0 qualifier), inExp)
        | ExprShape.ShapeVar var -> Expr.Var var
        | ExprShape.ShapeLambda(var, lambda) -> Expr.Lambda(var, go lambda)
        | ExprShape.ShapeCombination(combo, exp) -> ExprShape.RebuildShapeCombination(combo, List.map go exp)

    let get (expr: Expr) = go expr
