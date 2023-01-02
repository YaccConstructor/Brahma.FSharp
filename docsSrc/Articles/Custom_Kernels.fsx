(*** hide ***)
#I "../../src/Brahma.FSharp.OpenCL.Core/bin/Debug/net7.0"
#r "Brahma.FSharp.OpenCL.Core.dll"
#r "Brahma.FSharp.OpenCL.Shared.dll"

open Brahma.FSharp
open FSharp.Quotations


(**
# Custom kernels definition
*)

(**
## General rules

Custom kernels can be expressed as lambda functions inside F# quotation.
A lambda doesn't require particular solutions to mark it as a kernel,
but it must takes ndrange as a first parameter and returns unit.
Moreover, only curried format is supported.
*)

<@
    fun (range: Range1D) (buffer: int clarray) ->
        let gid = range.GlobalID0
        buffer.[gid] <- buffer.[gid] + 10
@>

(**
## Closure

You can also use variables defined outside the kernel lambda itself.
When compiled, they will be converted to constant literals.
*)

let constVal = 10

let parameterizedKernel (op: Expr<int -> int -> int>) =
    <@
        fun (range: Range1D) (buffer: int clarray) ->
            let gid = range.GlobalID0
            buffer.[gid] <- (%op) buffer.[gid] constVal
    @>

(**
## Generics

Generic kernels also supported.
*)

let genericKernel op =
    <@
        fun (range: Range1D) (buffer: 'a clarray) ->
            let gid = range.GlobalID0
            buffer.[gid] <- op buffer.[gid]
    @>
