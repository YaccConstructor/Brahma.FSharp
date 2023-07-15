module Brahma.FSharp.Tests.Translator.QuatationTransformation.Print

open Expecto
open Brahma.FSharp.OpenCL.Translator.QuotationTransformers

let private replaceTests =
    [ let createTest name source expected =
          test name {
              let actual = Print.replace source

              Expect.equal actual expected "Result should be the same."
          }

      let tpArgs: System.Type list = []
      let value = ""
      let bindArgs: Quotations.Expr list = []

      createTest "1 Test. Empty printf"
      <| <@ printf "" @>
      <| <@ Print.print tpArgs value bindArgs @>

      let tpArgs: System.Type list = []
      let value = "\\n"
      let bindArgs: Quotations.Expr list = []

      createTest "2 Test. Empty printfn"
      <| <@ printfn "" @>
      <| <@ Print.print tpArgs value bindArgs @>

      let tpArgs: System.Type list = []
      let value = "Hello, world!"
      let bindArgs: Quotations.Expr list = []

      createTest "3 Test. Hello, world! printf"
      <| <@ printf "Hello, world!" @>
      <| <@ Print.print tpArgs value bindArgs @>

      let tpArgs: System.Type list = []
      let value = "Hello, world!\\n"
      let bindArgs: Quotations.Expr list = []

      createTest "4 Test. Hello, world! printfn"
      <| <@ printfn "Hello, world!" @>
      <| <@ Print.print tpArgs value bindArgs @>

      let tpArgs: System.Type list = []
      let value = "He\\nllo, w\\nor\\nld!"
      let bindArgs: Quotations.Expr list = []

      createTest "5 Test. New line. printf"
      <| <@ printf "He\nllo, w\nor\nld!" @>
      <| <@ Print.print tpArgs value bindArgs @>

      let tpArgs: System.Type list = []
      let value = "He\\nllo, w\\nor\\nld!\\n"
      let bindArgs: Quotations.Expr list = []

      createTest "6 Test. New line. printfn"
      <| <@ printfn "He\nllo, w\nor\nld!" @>
      <| <@ Print.print tpArgs value bindArgs @>

      let tpArgs: System.Type list =
          [ typeof<int>
            typeof<int>
            typeof<string> ]

      let value = "%d %d %s"

      let bindArgs: Quotations.Expr list =
          [ <@@ 1 @@>
            <@@ 2 @@>
            <@@ "" @@> ]

      createTest "7 Test. %d %d %s. printf"
      <| <@ printf "%d %d %s" 1 2 "" @>
      <| <@ Print.print tpArgs value bindArgs @> ]

let tests = replaceTests |> testList "Printf" |> testSequenced
