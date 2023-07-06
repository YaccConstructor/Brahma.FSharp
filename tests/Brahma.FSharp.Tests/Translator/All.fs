module Brahma.FSharp.Tests.Translator.All

open Brahma.FSharp.OpenCL.Translator
open Brahma.FSharp.Tests.Translator
open Expecto

let translator = FSQuotationToOpenCLTranslator.CreateDefault()

let private common translator =
    [ BinOp.tests
      ControlFlow.tests
      NamesResolving.tests
      ConstantArray.tests
      LambdaLifting.tests
      Carrying.tests
      Injection.tests
      Printf.tests

      Specific.MergePath.tests ]
    |> List.map (fun f -> f translator)
    |> testList "Common"

let private union _ = [ Union.tests ] |> testList "Union"

let private transformation translator =
    [ QuatationTransformation.Transformation.tests
      QuatationTransformation.LambdaLifting.tests
      QuatationTransformation.VarDefsToLambda.tests ]
    |> List.map (fun f -> f translator)
    |> testList "Transformation"

let tests =
    [ common
      union
      transformation ]
    |> List.map (fun f -> f translator)
    |> testList "Translator"
