namespace Brahma.FSharp.OpenCL.Translator.QuotationTransformers

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Reflection
open Brahma.FSharp.OpenCL.Translator

module Print =
    module Utils =
        /// An active pattern to recognize any value expression
        /// which is an arbitrary depth subterm of the expression
        let rec (|HasValueAsSubExpr|_|) =
            function
            | Value x -> Some x
            | ExprShape.ShapeCombination(_, exprList) -> List.tryPick (|HasValueAsSubExpr|_|) exprList
            | _ -> None

        /// An active pattern to recognize lambda expression,
        /// that obtained from printf/printfn function.
        /// Example: printf "%d %f" -> ([Int, Float], "%d %f")
        let (|NewPrintfFormat|_|) =
            function
            | Call(None, mInfo, args) ->
                match mInfo.Name with
                | "PrintFormat"
                | "printfn" ->
                    let bindTypes =
                        match mInfo.ReturnType with
                        | _ when mInfo.ReturnType = typeof<unit> -> []
                        | _ when FSharpType.IsFunction mInfo.ReturnType -> Utils.getFunctionArgTypes mInfo.ReturnType
                        | _ -> failwithf "printf: returned type %A of NewPrintfFormat is not expected" mInfo.ReturnType

                    match args with
                    | [ HasValueAsSubExpr(s, _) ] ->
                        let s' = (s :?> string).Replace("\n", "\\n")
                        let s'' = if mInfo.Name = "printfn" then s' + "\\n" else s'

                        Some(bindTypes, s'')
                    | _ -> failwithf "printf: argument %A of NewPrintfFormat call is not expected" args
                | _ -> None
            | _ -> None

        let rec (|PartialPrintf|_|) =
            function
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

        let (|Printf|_|) =
            function
            | PartialPrintf(tpArgs, value, bindArgs) ->
                if List.length bindArgs = List.length tpArgs then
                    Some(tpArgs, value, bindArgs)
                else
                    None
            | _ -> None

    /// Function for replacing printf call
    let print (tpArgs: System.Type list) (value: string) (bindArgs: Expr list) = ()

    let rec replace =
        function
        | Utils.Printf(tpArgs, value, bindArgs) -> <@@ print tpArgs value bindArgs @@>
        | ExprShape.ShapeVar _ as expr -> expr
        | ExprShape.ShapeLambda(x, body) -> Expr.Lambda(x, replace body)
        | ExprShape.ShapeCombination(combo, exprList) ->
            ExprShape.RebuildShapeCombination(combo, List.map replace exprList)
