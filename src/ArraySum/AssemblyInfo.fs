namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("ArraySum")>]
[<assembly: AssemblyProductAttribute("Brahma.FSharp")>]
[<assembly: AssemblyDescriptionAttribute("F# quotation to OpenCL translator.")>]
[<assembly: AssemblyVersionAttribute("1.0.1")>]
[<assembly: AssemblyFileVersionAttribute("1.0.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0.1"
    let [<Literal>] InformationalVersion = "1.0.1"
