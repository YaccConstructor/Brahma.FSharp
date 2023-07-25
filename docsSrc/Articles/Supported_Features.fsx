(*** hide ***)
#I "../../src/Brahma.FSharp.OpenCL.Core/bin/Release/net7.0"
#r "Brahma.FSharp.OpenCL.Core.dll"
#r "Brahma.FSharp.OpenCL.Shared.dll"

open Brahma.FSharp
open FSharp.Quotations

(**
# Supported Features.

Translator supports only subset of F# language and this subset is described here.


## Basic constructions

### Array access
Array "by index" access is supported.

*)

<@
    fun (range: Range1D) (buf: ClArray<_>) ->
        buf.[1] <- buf.[0]
@>

(**
### Binding
Basic "let" binding is supported. Note, that now we support only "variable bindings". Nested functions, closures are not supported.
*)


<@
    fun (range: Range1D) (buf: ClArray<_>) ->
        let x = 1
        let y = (x - 1) * (x + 2)
        buf.[x] <- y
@>


(**
### Mutable binding
Mutability is available by using of "let mutable" binding.
*)


<@
    fun (range: Range1D) (buf: ClArray<_>) ->
        let mutable x = 1
        x <- x * 2
@>

(**
Note, that scopes are supported. So, you can "rebind" any name and "F#-style" visibility will be emuleted in target code. For example, next code will be translated correctly.
*)


<@
    fun (range: Range1D) (buf: ClArray<_>) ->
        let i = 2
        for i in 1 .. 3 do
            buf.[i] <- buf.[i] + 1
        buf.[0] <- i
@>


<@
    fun (range: Range1D) (buf: ClArray<_>) ->
        for i in 1 .. 3 do
            let i = i * 2
            buf.[i] <- 0
@>

(**
### Local funtions

You can also use local funtions inside kernel.
When compiled, they will be converted to OpenCL C functions.
*)

<@
    fun (range: Range1D) (buffer: int clarray) ->
        let gid = range.GlobalID0
        let f x = x + 10
        buffer.[gid] <- f buffer.[gid]
@>


(**
### Expression ignore by ```|> ignore```
*)


<@
    fun (range: Range1D) (buffer: int clarray) ->
        let gid = range.GlobalID0
        atomic inc buffer.[gid] |> ignore
@>

(**
## Control flow

Almost all basic control flow operators are supported.

### Sequential operations
*)


<@
    fun (range: Range1D) (buf: ClArray<int>) ->
        buf.[0] <- 2
        buf.[1] <- 4
@>

(**
### WHILE loop
*)


<@
    fun (range: Range1D) (buf: ClArray<_>) ->
        while buf.[0] < 5 do
            buf.[0] <- buf.[0] + 1
@>

(**
### FOR integer range loop
*)


<@
    fun (range: Range1D) (buf: ClArray<_>) ->
        for i in 1 .. 3 do
            buf.[i] <- 0
@>

(**
### Quotations injection
You can use "quotations injection" for code reusing or parameterization. For example, you can write something like this:
*)

let myFun = <@ fun x y -> y - x @>
let command =
    <@
        fun (range: Range1D) (buf: ClArray<int>) ->
            buf.[0] <- (%myFun) 2 5
            buf.[1] <- (%myFun) 4 9
    @>

let commandTemplate f =
    <@
        fun (range: Range1D) (buf: ClArray<int>) ->
            buf.[0] <- (%f) 2 5
            buf.[1] <- (%f) 4 9
    @>

let cmd1 = commandTemplate  <@ fun x y -> y - x @>
let cmd2 = commandTemplate  <@ fun x y -> y + x @>

(**
## Data types

Both host-kernel transferring and processing in kernels are supporeted for the following types.
- Primitive types
- Booleans
- Custom structs
- Records
- Tuples
- Discriminated unions

### Structs
*)

[<Struct>]
type MyStruct =
    val X: int
    val Y: int
    new (x, y) = { X = x; Y = y }
    new (x) = { X = x; Y = 0 }

let command1 =
    <@
        fun(range: Range1D) (buf: ClArray<int>) (str: MyStruct) ->
            buf.[0] <- str.X + str.Y
            let s2 = new MyStruct(6)
            let s3 = new MyStruct(s2.Y + 6)
            buf.[1] <- s2.X
    @>

let command2 =
    <@
        fun(range: Range1D) (buf: ClArray<int>) (arr: ClArray<MyStruct>) ->
            buf.[0] <- arr.[0].X
    @>

[<Struct>]
type StructOfIntInt64 =
    val mutable X: int
    val mutable Y: int64
    new(x, y) = { X = x; Y = y }

<@
    fun (range: Range1D) (buffer: ClArray<StructOfIntInt64>) ->
        let gid = range.GlobalID0
        let tmp = buffer.[gid]
        let x = tmp.X
        let y = tmp.Y
        let mutable innerStruct = StructOfIntInt64(x, y)
        innerStruct.X <- x
        innerStruct.Y <- y
        buffer.[gid] <- StructOfIntInt64(innerStruct.X, innerStruct.Y)
@>


(**
### Tuples
*)

<@
    fun (range: Range1D) (buf: ClArray<int>) (k1: int * int) (k2: int64 * byte) (k3: float32 * int) ->
        let x = fst k1
        buf.[0] <- x
        buf.[1] <- int (fst k3)
@>

<@
    fun (range: Range1D) (buf: ClArray<int>) ->
        let (a, b) = (1, 2)
        buf.[0] <- a
@>

(**
## Math operations
Supported functions from System.Math and Microsoft.FSharp.Core.Operators:
   * abs
   * acos
   * asin
   * atan
   * cos
   * cosh
   * exp
   * floor
   * log
   * log10
   * pow
   * sin
   * sinh
   * sqrt
   * tan
   * tanh

## OpenCL specific extensions

### Global and local memory space

Use `clarray` and `clcell` types to use data located in global memory inside the kernel.
Use `localArray` and `local` to allocate data in local memory inside the kernel.
*)

<@
    fun (range: Range1D) (globalBuffer: int clarray) ->
        let localBuffer = localArray<int> 10

        let gid = range.GlobalID0
        globalBuffer.[gid] <- globalBuffer.[gid] + 10
@>

(**
### Synchronization barriers

- `barrierLocal` corresponds to `barrier(CLK_LOCAL_MEM_FENCE)`
- `barrierGlobal` corresponds to `barrier(CLK_GLOBAL_MEM_FENCE)`
- `barrierFull` corresponds to `barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE)`
*)

(**
### Atomic functions

Use `atomic` for atomic function call.
*)

<@
    fun (range: Range1D) (buffer: int clarray) ->
        atomic (+) buffer.[0] 10 |> ignore
@>
