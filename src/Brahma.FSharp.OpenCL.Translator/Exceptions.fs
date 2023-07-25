namespace Brahma.FSharp.OpenCL.Translator

open System

/// The exception that is thrown when the kernel has invalid format.
type InvalidKernelException =
    inherit Exception

    new() = { inherit Exception() }  //
    new(message: string) = { inherit Exception(message) }
    new(message: string, inner: Exception) = { inherit Exception(message, inner) }  //

/// The exception that is thrown when the unexpected error occured during the translation.
type TranslationFailedException =
    inherit Exception

    new() = { inherit Exception() }  //
    new(message: string) = { inherit Exception(message) }
    new(message: string, inner: Exception) = { inherit Exception(message, inner) }
