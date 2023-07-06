open Expecto

open Brahma.FSharp.Tests

[<Tests>]
let allTests =
    testList
        "All tests"
        [ Translator.All.tests]
    |> testSequenced

open System.IO

[<EntryPoint>]
let main argv = allTests |> runTestsWithCLIArgs [] argv
