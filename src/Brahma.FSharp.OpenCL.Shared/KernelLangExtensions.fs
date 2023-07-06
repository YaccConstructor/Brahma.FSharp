namespace Brahma.FSharp

[<AutoOpen>]
type KernelLangExtensions =
    static member FailIfOutsideKernel() =
        failwith "Seems that you try to use openCL kernel function as regular F# function!"

[<AutoOpen>]
module KernelLangExtensions =
    let inline internal failIfOutsideKernel () =
        failwith "Seems that you try to use openCL kernel function as regular F# function!"

    let barrierLocal () =
        failIfOutsideKernel ()
        ignore null

    let barrierGlobal () =
        failIfOutsideKernel ()
        ignore null

    let barrierFull () =
        failIfOutsideKernel ()
        ignore null

    let local<'a> () =
        failIfOutsideKernel ()
        Unchecked.defaultof<'a>

    let localArray<'a> (size: int) =
        failIfOutsideKernel ()
        Unchecked.defaultof<array<'a>>

    let atomic (f: 'a -> 'b) =
        failIfOutsideKernel ()
        f

    let inline inc (p: 'a) =
        failIfOutsideKernel ()
        p + LanguagePrimitives.GenericOne<'a>

    let inline dec (p: 'a) =
        failIfOutsideKernel ()
        p - LanguagePrimitives.GenericOne<'a>

    // работает для всех типов
    let inline xchg (p: 'a) (value: 'a) =
        failIfOutsideKernel ()
        p

    let inline cmpxchg (p: 'a) (cmp: 'a) (value: 'a) =
        failIfOutsideKernel ()
        if p = cmp then value else p
