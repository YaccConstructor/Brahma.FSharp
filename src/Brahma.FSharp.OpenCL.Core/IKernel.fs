namespace Brahma.FSharp

open OpenCL.Net

/// Interface representing an OpenCL kernel.
type IKernel =
    abstract Kernel : Kernel
    abstract NDRange : INDRange
