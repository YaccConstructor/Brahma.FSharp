namespace Brahma.FSharp

open System

/// Interface representing n-dimensional index space.
type INDRange =
    abstract member GlobalWorkSize: IntPtr[]
    abstract member LocalWorkSize: IntPtr[]
    abstract member Dimensions: int

(*
    TODO + get_num_groups (uint dimindx)
    TODO + get_group_id (uint dimindx)
*)

/// Represents 1-dimensional index space.
type Range1D private (globalWorkSize: int, localWorkSize: int, __: unit) =
    /// <summary>
    /// Initializes a new instance of the <see cref="Range1D"/> class with specified global and local work size.
    /// </summary>
    /// <param name="globalWorkSize">Global work size to use.</param>
    /// <param name="localWorkSize">Local work size to use.</param>
    new(globalWorkSize: int, localWorkSize: int) = Range1D(globalWorkSize, localWorkSize, ())

    /// <summary>
    /// Initializes a new instance of the <see cref="Range1D"/> class with specified global work size and local work size setted to 1.
    /// </summary>
    /// <param name="globalWorkSize">Global work size to use.</param>
    new(globalWorkSize) = Range1D(globalWorkSize, 1)

    /// Gets the unique global work-item ID.
    member this.GlobalID0: int = FailIfOutsideKernel()

    /// Gets the unique local work-item ID.
    member this.LocalID0: int = FailIfOutsideKernel()

    member this.GlobalWorkSize = globalWorkSize
    member this.LocalWorkSize = localWorkSize

    interface INDRange with
        member this.GlobalWorkSize = [| IntPtr globalWorkSize |]
        member this.LocalWorkSize = [| IntPtr localWorkSize |]
        member this.Dimensions = 1

    /// <summary>
    /// Creates a new instance of the <see cref="Range1D"/> class with specified lower bound of global work size and local work size.
    /// The resulting global work size will be a multiple of local work size and not less than specified lower bound.
    /// </summary>
    /// <param name="neededSize">Lower bound of global work size.</param>
    /// <param name="localWorkSize">Local work size to use.</param>
    static member CreateValid(neededSize: int, localWorkSize: int) =
        let globalSize = (neededSize + localWorkSize - 1) / localWorkSize * localWorkSize
        Range1D(globalSize, localWorkSize)

/// Represents 2-dimensional index space.
type Range2D private (globalWorkSizeX: int, globalWorkSizeY: int, localWorkSizeX: int, localWorkSizeY: int, __: unit) =
    /// <summary>
    /// Initializes a new instance of the <see cref="Range2D"/> class with specified global and local work size.
    /// </summary>
    /// <param name="globalWorkSizeX">Global work size for dimension 0 to use.</param>
    /// <param name="globalWorkSizeY">Global work size for dimension 1 to use.</param>
    /// <param name="localWorkSizeX">Local work size for dimension 0 to use.</param>
    /// <param name="localWorkSizeY">Local work size for dimension 1 to use.</param>
    new(globalWorkSizeX: int, globalWorkSizeY: int, localWorkSizeX: int, localWorkSizeY: int) =
        Range2D(globalWorkSizeX, globalWorkSizeY, localWorkSizeX, localWorkSizeY, ())

    /// <summary>
    /// Initializes a new instance of the <see cref="Range2D"/> class with specified global and local work size setted to (1, 1).
    /// </summary>
    /// <param name="globalWorkSizeX">Global work size for dimension 0 to use.</param>
    /// <param name="globalWorkSizeY">Global work size for dimension 1 to use.</param>
    new(globalWorkSizeX, globalWorkSizeY) = Range2D(globalWorkSizeX, globalWorkSizeY, 1, 1)

    /// Gets the unique global work-item ID for dimension 0.
    member this.GlobalID0: int = FailIfOutsideKernel()

    /// Gets the unique global work-item ID for dimension 1.
    member this.GlobalID1: int = FailIfOutsideKernel()

    /// Gets the unique local work-item ID for dimension 0.
    member this.LocalID0: int = FailIfOutsideKernel()

    /// Gets the unique local work-item ID for dimension 1.
    member this.LocalID1: int = FailIfOutsideKernel()

    member this.GlobalWorkSize = (globalWorkSizeX, globalWorkSizeY)
    member this.LocalWorkSize = (localWorkSizeX, localWorkSizeY)

    interface INDRange with
        member this.GlobalWorkSize = [| IntPtr globalWorkSizeX; IntPtr globalWorkSizeY |]

        member this.LocalWorkSize = [| IntPtr localWorkSizeX; IntPtr localWorkSizeY |]

        member this.Dimensions = 2

/// Represents 3-dimensional index space.
type Range3D
    private
    (
        globalWorkSizeX: int,
        globalWorkSizeY: int,
        globalWorkSizeZ: int,
        localWorkSizeX: int,
        localWorkSizeY: int,
        localWorkSizeZ: int,
        __: unit
    ) =
    /// <summary>
    /// Initializes a new instance of the <see cref="Range3D"/> class with specified global and local work size.
    /// </summary>
    /// <param name="globalWorkSizeX">Global work size for dimension 0 to use.</param>
    /// <param name="globalWorkSizeY">Global work size for dimension 1 to use.</param>
    /// <param name="globalWorkSizeZ">Global work size for dimension 2 to use.</param>
    /// <param name="localWorkSizeX">Local work size for dimension 0 to use.</param>
    /// <param name="localWorkSizeY">Local work size for dimension 1 to use.</param>
    /// <param name="localWorkSizeZ">Local work size for dimension 2 to use.</param>
    new(globalWorkSizeX: int, globalWorkSizeY: int, globalWorkSizeZ: int, localWorkSizeX: int, localWorkSizeY: int, localWorkSizeZ: int) =
        Range3D(globalWorkSizeX, globalWorkSizeY, globalWorkSizeZ, localWorkSizeX, localWorkSizeY, localWorkSizeZ)

    /// <summary>
    /// Initializes a new instance of the <see cref="Range3D"/> class with specified global and local work size setted to (1, 1, 1).
    /// </summary>
    /// <param name="globalWorkSizeX">Global work size for dimension 0 to use.</param>
    /// <param name="globalWorkSizeY">Global work size for dimension 1 to use.</param>
    /// <param name="globalWorkSizeZ">Global work size for dimension 2 to use.</param>
    new(globalWorkSizeX, globalWorkSizeY, globalWorkSizeZ) = Range3D(globalWorkSizeX, globalWorkSizeY, globalWorkSizeZ, 1, 1, 1)

    /// Gets the unique global work-item ID for dimension 0.
    member this.GlobalID0: int = FailIfOutsideKernel()

    /// Gets the unique global work-item ID for dimension 1.
    member this.GlobalID1: int = FailIfOutsideKernel()

    /// Gets the unique global work-item ID for dimension 2.
    member this.GlobalID2: int = FailIfOutsideKernel()

    /// Gets the unique local work-item ID for dimension 0.
    member this.LocalID0: int = FailIfOutsideKernel()

    /// Gets the unique local work-item ID for dimension 1.
    member this.LocalID1: int = FailIfOutsideKernel()

    /// Gets the unique local work-item ID for dimension 2.
    member this.LocalID2: int = FailIfOutsideKernel()

    member this.GlobalWorkSize = (globalWorkSizeX, globalWorkSizeY, globalWorkSizeZ)
    member this.LocalWorkSize = (localWorkSizeX, localWorkSizeY, localWorkSizeZ)

    interface INDRange with
        member this.GlobalWorkSize =
            [| IntPtr globalWorkSizeX; IntPtr globalWorkSizeY; IntPtr globalWorkSizeZ |]

        member this.LocalWorkSize =
            [| IntPtr localWorkSizeX; IntPtr localWorkSizeY; IntPtr globalWorkSizeZ |]

        member this.Dimensions = 3
