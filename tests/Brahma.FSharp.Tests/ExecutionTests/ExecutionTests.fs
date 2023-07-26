module ExecutionTests

open Brahma.FSharp.OpenCL.Translator
open Expecto
open Brahma.FSharp
open Brahma.FSharp.OpenCL.Shared

// TODO make it lazy?
let allContexts =
    ClDevice.GetAvailableDevices()
    |> Seq.map (fun device ->
        let translator = FSQuotationToOpenCLTranslator(device)
        let clContext = ClContext(device, translator)
        RuntimeContext(clContext)
    )

let tests =
    [
        for context in allContexts do
            yield!
                [
                    testList $"System tests with running kernels on %A{context}"
                    <| RuntimeTests.tests context
                    testList $"Compilation tests on %A{context}" <| CompilationTests.tests context
                    testList $"Tests on 'opencl' computation exression on %A{context}"
                    <| WorkflowBuilderTests.tests context
                    ptestList $"Tests on atomic functions on %A{context}"
                    <| AtomicTests.tests context
                    testList $"Tests on composite types on %A{context}"
                    <| CompositeTypesTests.tests context
                ]
    ]
