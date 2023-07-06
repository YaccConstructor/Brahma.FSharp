open Expecto

open Brahma.FSharp.Tests

[<Tests>]
let allTests =
    testList
        "All tests"
        [ Translator.All.tests
          testList "Execution tests" ExecutionTests.tests ]
    |> testSequenced

[<EntryPoint>]
let main argv = allTests |> runTestsWithCLIArgs [] argv
