open Expecto

open Brahma.FSharp.Tests

[<Tests>]
let allTests =
    testList
        "All tests"
        [ Translator.All.tests
          ExecutionTests.tests |> testList "Execution" ]
    |> testSequenced

[<EntryPoint>]
let main argv = allTests |> runTestsWithCLIArgs [] argv
