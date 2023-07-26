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

    let (|LetFunc|_|) exp = letDefinition Utils.isFunction exp

    let (|LetVar|_|) (expr: Expr) =
        letDefinition (not << Utils.isFunction) expr

    /// let f x1 x2 x3 = body in e
    /// => LetFuncUncurry(f, [x1; x2, x3], body, e)
    let (|LetFuncUncurry|_|) (expr: Expr) =
        match expr with
        | Let(var, DerivedPatterns.Lambdas(args, body), inExp) ->
            let args = List.concat args
            Some(var, args, body, inExp)
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
