module Brahma.FSharp.Tests.Translator.QuatationTransformation.Common

open Expecto
open FSharp.Quotations
open Brahma.FSharp.OpenCL.Translator

[<AutoOpen>]
module Helpers =
    let equalsMessage = "Values should be the same."

    let rec renameUnitVar (expr: Expr) =
        expr.Substitute
        <| function
            | var when var.Type.IsEquivalentTo(typeof<unit>) ->
                Var("unitVar", var.Type, var.IsMutable)
                |> Expr.Var
                |> Some
            | _ -> None

    let var<'t> name = Var(name, typeof<'t>)

    let expVar<'t> name = Expr.Cast<'t>(Expr.Var(var<'t> name))

    let varEqual (actual: Var) (expected: Var) =
        Expect.equal actual.IsMutable expected.IsMutable "Mutability must be the same"
        Expect.isTrue (actual.Type.IsEquivalentTo(expected.Type)) "Type must be the same"
        Expect.equal actual.Name expected.Name "Names must be the same"

    let openclTransformQuotation (translator: FSQuotationToOpenCLTranslator) (expr: Expr) =
        translator.TransformQuotation expr

    let equalAsStrings (actual: Expr) (expected: Expr) (msg: string) =
        Expect.equal <| actual.ToString() <| expected.ToString() <| msg

    let inline typesEqual
        (actual: ^a when ^a : (member Type : System.Type))
        (expected: ^b when ^b : (member Type : System.Type)) =

        Expect.isTrue (actual.Type = expected.Type) "Types must be the same"

    let equalToTheExactUnitVars (actual: Expr) (expected: Expr) (msg: string) =
        let actual = renameUnitVar actual
        let expected = renameUnitVar expected

        equalAsStrings actual expected msg

    let assertMethodEqual (actual: Var * Expr) (expected: Var * Expr) =
        Expect.equal (fst actual).Name (fst expected).Name "Method names should be equal"

        equalAsStrings (snd actual) (snd expected)
        <| $"Method bodies of %s{(fst actual).Name} is not equal"
