namespace Brahma.FSharp.OpenCL.Translator

open Brahma.FSharp.OpenCL.AST

[<AutoOpen>]
module Bindings =
    [<Literal>]
    let Range1D_ = "range1d"

    [<Literal>]
    let Range2D_ = "range2d"

    [<Literal>]
    let Range3D_ = "range3d"

    [<Literal>]
    let ClArray_ = "clarray"

    [<Literal>]
    let ClCell_ = "clcell"

    [<Literal>]
    let IBuffer_ = "ibuffer"

    type BoolHostAlias = byte
    let BoolClAlias = UChar

module internal Anchors =
    let _localID0 = Unchecked.defaultof<int>

    let _globalSize0 = Unchecked.defaultof<int>
    let _globalSize1 = Unchecked.defaultof<int>
    let _globalSize2 = Unchecked.defaultof<int>

    let _localSize0 = Unchecked.defaultof<int>
    let _localSize1 = Unchecked.defaultof<int>
    let _localSize2 = Unchecked.defaultof<int>
