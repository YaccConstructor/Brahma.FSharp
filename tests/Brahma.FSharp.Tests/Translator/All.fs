module Brahma.FSharp.Tests.Translator.All

open Brahma.FSharp.Tests.Translator
open Expecto

let translator =
    Brahma.FSharp.OpenCL.Translator.FSQuotationToOpenCLTranslator.CreateDefault()

let common =
    [
        BinOp.tests
        ControlFlow.tests
        NamesResolving.tests
        ConstantArray.tests
        LambdaLifting.tests
        Carrying.tests
        Injection.tests

        Specific.MergePath.tests
    ]
    |> testList "Common"

let extensions =
    [
        LangExtensions.Barrier.tests
        LangExtensions.LocalId.tests
        LangExtensions.LocalMemory.tests
        LangExtensions.WorkSize.tests
    ]
    |> testList "LangExtensions"

let passes =
    [
        QuatationTransformation.Print.tests
        QuatationTransformation.WorkSize.tests
        QuatationTransformation.Names.tests
        QuatationTransformation.Variables.tests
        QuatationTransformation.VarToRef.tests
        QuatationTransformation.Lifting.tests
    ]
    |> testList "Passes"

let union = [ Union.tests ] |> testList "Union"

let transformation =
    [ QuatationTransformation.Transformation.tests ] |> testList "Transformation"

let tests = [ common; passes; union; transformation ] |> testList "Translator"
