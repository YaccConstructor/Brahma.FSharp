namespace Brahma.FSharp.OpenCL.Translator

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection

[<AutoOpen>]
module Extensions =
    type Expr with

        /// Builds an expression that represents the lambda
        static member Lambdas(args: Var list list, body: Expr) =
            let mkRLinear mk (vs, body) =
                List.foldBack (fun v acc -> mk (v, acc)) vs body

            let mkTupledLambda (args, body) =
                match args with
                | [ x ] -> Expr.Lambda(x, body)
                | [] -> Expr.Lambda(Var("unitVar", typeof<unit>), body)
                | _ ->
                    let tupledArg =
                        Var("tupledArg", FSharpType.MakeTupleType(args |> List.map (fun v -> v.Type) |> List.toArray))

                    Expr.Lambda(
                        tupledArg,
                        (args, [ 0 .. args.Length - 1 ], body)
                        |||> List.foldBack2 (fun var idxInTuple letExpr ->
                            Expr.Let(var, Expr.TupleGet(Expr.Var tupledArg, idxInTuple), letExpr))
                    )

            mkRLinear mkTupledLambda (args, body)
