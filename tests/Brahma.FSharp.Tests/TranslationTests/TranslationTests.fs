module TranslationTests

open Brahma.FSharp.OpenCL.Translator
open Expecto

let translators = [
    FSQuotationToOpenCLTranslator.CreateDefault()
]

let tests = [
    for translator in translators do yield! [
        testList "Tests for translator" <| TranslatorTests.tests translator
        testList "Quotation transformer tests" <| QuotationTransformersTests.tests translator
    ]
]
