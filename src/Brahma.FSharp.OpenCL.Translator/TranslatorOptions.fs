namespace Brahma.FSharp.OpenCL.Translator

type TranslatorOptions() =
    member val UseNativeBooleanType = false with get, set
    member val BoolAsBit = false with get, set
