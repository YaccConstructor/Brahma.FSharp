namespace Brahma.FSharp.OpenCL.Translator.QuotationTransformers

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Reflection
open FSharp.Core.LanguagePrimitives
open Brahma.FSharp.OpenCL.Translator

module Patterns =
    let rec (|HasSubExpr|_|) ((|Pattern|_|): Expr -> 'a Option) expr =
        match expr with
        | Pattern x -> Some x
        | ExprShape.ShapeCombination(shapeObj, exprList) ->
            exprList
            |> List.map ((|HasSubExpr|_|) (|Pattern|_|))
            |> List.fold
                (fun x y ->
                    match x with
                    | Some _ -> x
                    | None -> y)
                None
        | _ -> None

    /// An active pattern to recognize any value expression
    /// which is an arbitrary depth subterm of the expression
    let (|HasValueAsSubExpr|_|) (expr: Expr) = (|HasSubExpr|_|) (|Value|_|) expr

    /// An active pattern to recognize lambda expression,
    /// that obtained from printf/printfn function.
    /// Example: printf "%d %f" -> ([Int, Float], "%d %f")
    let (|NewPrintfFormat|_|) (expr: Expr) =
        match expr with
        | Call(None, mInfo, args) ->
            match mInfo.Name with
            | "PrintFormat"
            | "printfn" ->
                let retType = mInfo.ReturnType

                let bindTypes =
                    match retType with
                    | _ when retType = typeof<unit> -> []
                    | _ when FSharpType.IsFunction retType -> Utils.getFunctionArgTypes <| mInfo.ReturnType
                    | _ -> failwithf "printf: returned type %A of NewPrintfFormat is not expected" retType

                match args with
                | [ HasValueAsSubExpr(s, _) ] ->
                    let s' = (s :?> string).Replace("\n", "\\n")
                    let s'' = if mInfo.Name = "printfn" then s' + "\\n" else s'
                    Some(bindTypes, s'')
                | _ -> failwithf "printf: argument %A of NewPrintfFormat call is not expected" args
            | _ -> None
        | _ -> None

    let rec (|PartialPrintf|_|) (expr: Expr) =
        match expr with
        | Let(_, value, inExpr) ->
            match value with
            | NewPrintfFormat(tpArgs, value) ->
                assert (tpArgs = Utils.getFunctionArgTypes inExpr.Type)
                Some(tpArgs, value, [])
            | _ -> None
        | Application(f, arg) ->
            match f with
            | PartialPrintf(tpArgs, value, bindArgs) -> Some(tpArgs, value, bindArgs @ [ arg ])
            | _ -> None
        | NewPrintfFormat(tpArgs, formatStr) -> Some(tpArgs, formatStr, [])
        | _ -> None

    let (|Printf|_|) (expr: Expr) =
        match expr with
        | PartialPrintf(tpArgs, value, bindArgs) ->
            if List.length bindArgs = List.length tpArgs then
                Some(tpArgs, value, bindArgs)
            else
                None
        | _ -> None

    let private letDefinition (predicate: Var -> bool) (expr: Expr) =
        match expr with
        | Let(var, expr, inExpr) -> if predicate var then Some(var, expr, inExpr) else None
        | _ -> None

    let (|LetFunc|_|) (expr: Expr) = letDefinition Utils.isFunction expr

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
