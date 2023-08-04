module Brahma.FSharp.Tests.Translator.All

open Brahma.FSharp.Tests.Translator
open Expecto

let common =
    [
        BinOp.tests
        ControlFlow.tests
        NamesResolving.tests
        ConstantArray.tests
        LambdaLifting.tests
        Carrying.tests
        Injection.tests
        Printf.tests
        Union.tests

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
        QuatationTransformation.Atomic.tests
    ]
    |> testList "Passes"

let transformation =
    [ QuatationTransformation.Transformation.tests ] |> testList "Transformation"

let tests = [ common; extensions; passes; transformation ] |> testList "Translator"
