open Expecto

open Brahma.FSharp.Tests

[<Tests>]
let allTests =
    testList
        "All tests"
        [ Brahma.FSharp.Tests.Translator.Carrying.tests
          testList "Execution tests" ExecutionTests.tests ]
    |> testSequenced

[<EntryPoint>]
let main argv = allTests |> runTestsWithCLIArgs [] argv
