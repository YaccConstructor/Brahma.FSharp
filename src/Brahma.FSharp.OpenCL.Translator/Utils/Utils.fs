namespace Brahma.FSharp.OpenCL.Translator

open Microsoft.FSharp.Quotations
open System

module Utils =
    let getMethodInfoOfCall (expr: Expr) =
        match expr with
        | Patterns.Call(_, mInfo, _) -> mInfo
        | DerivedPatterns.Lambdas(_, Patterns.Call(_, mInfo, _)) -> mInfo
        | _ -> failwithf $"Expression is not kind of call, but {expr}"

    let makeGenericMethodCall (types: Type list) (expr: Expr) =
        (getMethodInfoOfCall expr)
            .GetGenericMethodDefinition()
            .MakeGenericMethod(Array.ofList types)

    let hasAttribute<'attr> (tp: Type) =
        tp.GetCustomAttributes(false)
        |> Seq.exists (fun attr -> attr.GetType() = typeof<'attr>)

    let roundUp n x = if x % n <> 0 then (x / n) * n + n else x
