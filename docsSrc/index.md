# Brahma.FSharp

---

## What is Brahma.FSharp?

Brahma.FSharp is a way to use GPGPUs in your F# projects. Brahma.FSharp is based on F# code quotations to OpenCL C translation. Brahma.FSharp provides not only translator but also memory management system and host-devise communication system.

## Why use Brahma.FSharp?

 * Brahma.FSharp uses OpenCL for communication with GPU. So, you can utilize not only NVIDIA hardware but any device, which supports OpenCL (e.g. AMD or Intel devices).
 * Brahma.FSharp supports not only primitive types, but also tuples, structures, and discriminated unions.
 * Brahma.FSharp not limited by imperative subset of F#. It supports nested bindings, local functions, pattern matching.
 * Communication with GPGPUs based on Mailbox processors, thus asynchronous communications, which are native for GPGPU computations, can be used easily.
 
---

<!--div class="row row-cols-1 row-cols-md-2">
  <div class="col mb-4">
    <div class="card h-100">
      <div class="card-body">
        <h5 class="card-title">Articles</h5>
        <p class="card-text">Step-by-step examples, features description, troubleshooting, etc.</p>
      </div>
      <div class="card-footer text-right   border-top-0">
        <a href="{{siteBaseUrl}}/Articles/index.html" class="btn btn-primary">Get started</a>
      </div>
    </div>
  </div>
  <div class="col mb-4">
    <div class="card h-100">
      <div class="card-body">
        <h5 class="card-title">Api Reference</h5>
        <p class="card-text">Contain technical reference for APIs.</p>
      </div>
      <div class="card-footer text-right   border-top-0">
        <a href="{{siteBaseUrl}}/Api_Reference/Brahma.FSharp/Brahma.FSharp.OpenCL.Translator/Brahma.FSharp.OpenCL.Translator.html" class="btn btn-primary">Read Api Docs</a>
      </div>
    </div>
  </div>
</div-->
