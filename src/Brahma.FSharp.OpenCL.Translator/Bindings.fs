namespace Brahma.FSharp.OpenCL.Translator

open Brahma.FSharp.OpenCL.AST

[<AutoOpen>]
module Bindings =
    let [<Literal>] Range1D_ = "range1d"
    let [<Literal>] Range2D_ = "range2d"
    let [<Literal>] Range3D_ = "range3d"

    let [<Literal>] ClArray_ = "clarray"
    let [<Literal>] ClCell_ = "clcell"
    let [<Literal>] IBuffer_ = "ibuffer"

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
