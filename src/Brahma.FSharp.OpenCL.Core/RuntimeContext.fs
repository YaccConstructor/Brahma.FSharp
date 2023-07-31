namespace Brahma.FSharp

type RuntimeOptions =
    {
        // TODO if 2D or 3D
        WorkgroupSize: int
    }

    static member Default = { WorkgroupSize = 256 }

/// Provides a context to run OpenCL computation.
type RuntimeContext(clContext: ClContext) =
    let mutable runtimeOptions = RuntimeOptions.Default

    let mutable queue = clContext.QueueProvider.CreateQueue()

    new(clDevice: ClDevice) = RuntimeContext(ClContext(clDevice))

    member this.RuntimeOptions
        with get () = runtimeOptions
        and private set (value) = runtimeOptions <- value

    member this.CommandQueue
        with get () = queue
        and private set (value) = queue <- value

    member this.ClContext = clContext

    member internal this.WithNewCommandQueue() =
        RuntimeContext(clContext, RuntimeOptions = this.RuntimeOptions)

    member internal this.WithRuntimeOptions(runtimeOptions) =
        RuntimeContext(clContext, RuntimeOptions = runtimeOptions, CommandQueue = this.CommandQueue)

    override this.ToString() = clContext.ToString()
