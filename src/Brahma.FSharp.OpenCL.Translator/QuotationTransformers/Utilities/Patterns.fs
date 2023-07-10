namespace Brahma.FSharp.OpenCL.Translator.QuotationTransformers

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Core.LanguagePrimitives
open Brahma.FSharp.OpenCL.Translator

module Patterns =
    let private letDefinition (predicate: Var -> bool) =
        function
        | Let(var, expr, inExpr) -> if predicate var then Some(var, expr, inExpr) else None
        | _ -> None

    let (|LetFunc|_|) = letDefinition Utils.isFunction

    let (|LetVar|_|) (expr: Expr) =
        letDefinition (not << Utils.isFunction) expr

    // HACK это все можно DerrivedPatterns.Lambdas и DerrivedPatterns.Applications заменить же
    let rec private uncurryLambda (expr: Expr) =
        match expr with
        | ExprShape.ShapeLambda(var, body) ->
            let (args, innerBody) = uncurryLambda body
            var :: args, innerBody
        | _ -> [], expr

    let private uncurryApplication (expr: Expr) =
        let rec uncurryApplicationImpl (acc: list<Expr>) (expr: Expr) =
            match expr with
            | Application(l, r) -> uncurryApplicationImpl (r :: acc) l
            | _ -> expr, acc

        uncurryApplicationImpl [] expr

    /// let f x1 x2 x3 = body in e
    /// => LetFuncUncurry(f, [x1; x2, x3], body, e)
    let (|LetFuncUncurry|_|) (expr: Expr) =
        match expr with
        | LetFunc(var, body, inExpr) ->
            let args, body' = uncurryLambda body
            Some(var, args, body', inExpr)
        | _ -> None

    /// e0 e1 e2 e3
    /// => (e0, [e1; e2; e3])
    let (|ApplicationUncurry|_|) (expr: Expr) =
        // TODO: think about partial function, we should to raise exception somewhere
        match expr with
        | Application _ -> Some <| uncurryApplication expr
        | _ -> None

    let (|GlobalVar|_|) =
        function
        | Patterns.PropertyGet(Some(Patterns.Var v), propInfo, args) when
            v.Type.Name.ToLower().StartsWith ClArray_
            && propInfo.Name.ToLower().StartsWith "item"
            || v.Type.Name.ToLower().StartsWith ClCell_
               && propInfo.Name.ToLower().StartsWith "value"
            ->
            Some v
        | _ -> None

    let (|ValidVolatileArg|_|) =
        function
        // global
        | GlobalVar v -> Some v
        // non-global
        | Patterns.Var var
        | DerivedPatterns.SpecificCall <@ IntrinsicFunctions.GetArray @> (_, _, [ Patterns.Var var; _ ]) -> Some var
        | _ -> None
