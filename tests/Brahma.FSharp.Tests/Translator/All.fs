module Brahma.FSharp.Tests.Translator.All

open Brahma.FSharp.OpenCL.Translator
open Brahma.FSharp.Tests.Translator
open Expecto

let translator = FSQuotationToOpenCLTranslator.CreateDefault()

let private common =
    [ BinOp.tests
      ControlFlow.tests
      NamesResolving.tests
      ConstantArray.tests
      LambdaLifting.tests
      Carrying.tests
      Injection.tests
      Printf.tests

      Specific.MergePath.tests ]
    |> testList "Common"

let private union = [ Union.tests ] |> testList "Union"

let private transformation =
    [ QuatationTransformation.Transformation.tests
      QuatationTransformation.LambdaLifting.tests
      QuatationTransformation.VarDefsToLambda.tests ]
    |> testList "Transformation"

let tests =
    [ common
      union
      transformation ]
    |> testList "Translator"
