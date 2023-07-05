module TranslationTests

open Brahma.FSharp.OpenCL.Translator
open Expecto

let translators = [
    FSQuotationToOpenCLTranslator.CreateDefault()
]
