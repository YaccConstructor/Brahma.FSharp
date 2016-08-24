[<AutoOpen>]
module OpenCL

let private kFail () = failwith "OpenCL kernel function!"

/// Alias for atom_add. Not returns old value in F#.
/// ### Example
/// a.[i] <!+ buf
let (<!+) a b =
    kFail ()
    a + b |> ignore

/// Alias for atom_add. Returns old value.
/// ### Example
/// let oldV = a.[i] <!+> buf
let (<!+>) a b = 
    kFail ()
    a + b

/// Alias for atom_sub. Not returns old value in F#. 
/// ### Example
/// a.[i] <!- buf
let (<!-) a b =
    kFail ()
    a - b |> ignore

/// Alias for atom_sub. Returns old value.
/// ### Example
/// let oldV = a.[i] <!-> buf
let (<!->) a b =
    kFail ()
    a - b

/// Alias for atom_xchg. Not returns old value in F#
/// ### Example
/// a.[i] <! buf
let (<!) (a:'a) (b:'a) =
    kFail ()
    b |> ignore 

/// Alias for atom_xchg. Returns old value.
/// ### Example
/// let oldV = a.[i] <!> buf
let (<!>) (a:'a) (b:'a) = 
    kFail ()
    b 

//let (<&&>) (a:uint16) b = 
//    kFail ()
//    a &&& b
//
//let (<&&>) (a:int) b = 
//    kFail ()
//    a &&& b
let aIncrR a = 
    kFail ()
    a + 1

let aIncr a = 
    kFail ()
    a + 1
    |> ignore

let aDecr a = 
    kFail ()
    a - 1
    |> ignore

let aDecrR a = 
    kFail ()
    a - 1

let aMax a b = 
    kFail ()
    max a b |> ignore

let aMaxR a b = 
    kFail ()
    max a b

let aMin a b = 
    kFail ()
    min a b |> ignore

let aMinR a b = 
    kFail ()
    min a b

let aCompExch a b c =
    kFail ()
    if a = b
    then c
    else a
    |> ignore

let aCompExchR a b c =
    kFail ()
    if a = b
    then c
    else a
    |> ignore
    a

let local a = a
let barrier () = ignore(null)

let _byte (x:bool) = 0uy

let as_uint (b1:byte) (b2:byte) (b3:byte) (b4:byte) = uint32 1
