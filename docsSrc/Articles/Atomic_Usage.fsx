(*** hide ***)
#I "../../src/Brahma.FSharp.OpenCL.Core/bin/Release/net7.0"
#r "Brahma.FSharp.OpenCL.Core.dll"
#r "Brahma.FSharp.OpenCL.Shared.dll"

open Brahma.FSharp

(**
# Atomic Usage [Experimental]

This feature is unstable and behaviour depends on hardware and driver version/vendor.

*)

(**
## Target functions
*)

(**
As target function for ```atomic``` it`s possible to use anonymous lambda functions.
*)

<@
    fun (range: Range1D) (buffer: int clarray) ->
        atomic (+) buffer.[0] 1 |> ignore
@>

(**
You can use quoted lambdas that inserted inside kernel quotation by splicing operator.
*)

let f = <@ fun x y -> x + 2 * y @>

<@
    fun (range: Range1D) (buffer: int clarray) ->
        atomic %f buffer.[0] 1 |> ignore
@>

(**
You cannot use arbitary named functions in atomic expressions, but you can use any named function, generally supported by Brahma.FSharp.
*)

<@
    fun (range: Range1D) (buffer: int clarray) ->
        atomic min buffer.[0] 0 |> ignore
@>

(**
There are some specific named functions, which can only be used as target function in atomic expression:

- ```inc``` (only ```int```, ```uint32```, ```int64```, ```uint64``` supported)
- ```dec``` (only ```int```, ```uint32```, ```int64```, ```uint64``` supported)
- ```xchg``` (arbitary types supported)
- ```cmpxchg``` (arbitary types supported)
*)

<@
    fun (range: Range1D) (buffer: int clarray) ->
        atomic inc buffer.[0] |> ignore
@>

<@
    fun (range: Range1D) (buffer: bool clarray) ->
        atomic xchg buffer.[0] false |> ignore
@>

(**
You can use srtp atomic functions in generic kernels. Following kernel can be used both with ```int``` and ```float32```.
*)

let inline kernel () =
    <@
        fun (range: Range1D) (result: 'a clarray) (value: 'a) ->
            atomic (+) result.[0] value |> ignore
    @>

(**
## Other rules
*)

(**
- The first argument of target function must be te same type as return type of the function.
- The first argument of target function must be od the form ```var``` or ```var.[expr]``` where ```var``` is variable in global or local memory.
- You can use atomic functions inside other functions, but the first argument must be provided.
*)

<@
    fun (range: Range1D) (buffer: int clarray) ->
        let g x = atomic (+) buffer.[0] x
        g 1 |> ignore
@>

(**
But following kernels is not supported:
*)

<@
    fun (range: Range1D) (buffer: int clarray) ->
        // function without application of first parameter
        let g x y = atomic (+) x y
        g buffer.[0] 1 |> ignore
@>

<@
    fun (range: Range1D) (result: int clarray) ->
        // implicit argument
        let g = atomic (+) result.[0]
        g 1 |> ignore
@>

<@
    fun (range: Range1D) (result: int clarray) ->
        // function's argument in closure of target function
        let g y = atomic (fun x -> x + y + 1) result.[0]
        g 1 |> ignore
@>

(**
## Usage advices
*)

(**
Use
*)

<@
    fun (range: Range1D) (buffer: int clarray) ->
        atomic (+) buffer.[0] 1 |> ignore
@>

(**
instead of
*)

<@
    fun (range: Range1D) (buffer: int clarray) ->
        atomic (fun x -> x + 1) buffer.[0] |> ignore
@>
