(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
## OpenCL specific operations

 * [Data transfer operations](reference/brahma-fsharp-opencl-extensions.html)
 * [Supported kernel operations](reference/global-opencl.html).
 * Supported functions from System.Math and Microsoft.FSharp.Core.Operators: abs, acos, asin, atan, cos, cosh, exp, floor, log, log10, pow, sin, sinh, sqrt, tan, tanh


## Basic constructions.

### Array access
Array "by index" access is supported.
*)

let command = 
    <@ 
        fun (range:_1D) (buf:array<_>) ->
            buf.[1] <- buf.[0]
    @>
(**
### Binding
Basic "let" binding is supported. Note, that now we support only "variable bindings". Nested functions, closures are not supported.
*)

let command = 
    <@ 
        fun (range:_1D) (buf:array<_>) ->
            let x = 1
            let y = (x -1) * (x + 2)
    @>

(**
### Mutable binding
Mutability is available by using of "let mutable" binding.
*)

let command = 
    <@ 
        fun (range:_1D) (buf:array<_>) ->
            let x = 1
            x <- x * 2
    @>

(**
Note, that scoupes are supported. So, you can "rebind" any name and "F#-style" visibility will be emuleted in target code. For example, next code will be translated correctly.
*)

let command = 
    <@ 
        fun (range:_1D) (buf:array<_>) ->
            let i = 2
            for i in 1..3 do     
                buf.[i] <- buf.[i] + 1
            buf.[0] <- i
    @>

let command = 
    <@ 
        fun (range:_1D) (buf:array<_>) ->
            for i in 1..3 do
                let i = i * 2     
                buf.[i] <- 0
    @>

(**
##Control flow

### Sequential opertions
*)

let command = 
    <@ 
        fun (range:_1D) (buf:array<int>) ->
            buf.[0] <- 2
            buf.[1] <- 4
    @>
(**
### WHILE loop
*)

let command = 
    <@ 
        fun (range:_1D) (buf:array<_>) ->
         while buf.[0] < 5 do
             buf.[0] <- buf.[0] + 1
    @>

(**
### FOR integer range loop
*)

let command = 
    <@ 
        fun (range:_1D) (buf:array<_>) -> 
            for i in 1..3 do buf.[i] <- 0
    @>

(**
### Quotations injection
You can use "quotations injection" for code reusing or parameterization. For example, you can write something like this:
*)

let myFun = <@ fun x y -> y - x @>
let command = 
    <@ 
        fun (range:_1D) (buf:array<int>) ->
            buf.[0] <- (%myFun) 2 5
            buf.[1] <- (%myFun) 4 9
    @>

let commandTeplate f = 
    <@ 
        fun (range:_1D) (buf:array<int>) ->
            buf.[0] <- (%f) 2 5
            buf.[1] <- (%f) 4 9
    @>

let cmd1 = commandTeplate  <@ fun x y -> y - x @>
let cmd2 = commandTeplate  <@ fun x y -> y + x @>


(**
Some more info
*)
