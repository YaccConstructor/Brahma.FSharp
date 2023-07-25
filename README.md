# Brahma.FSharp 

[![FAKE Build](https://github.com/YaccConstructor/Brahma.FSharp/actions/workflows/build-on-push.yml/badge.svg)](https://github.com/YaccConstructor/Brahma.FSharp/actions/workflows/build-on-push.yml) 
[![NuGet Badge](https://buildstats.info/nuget/Brahma.FSharp)](https://www.nuget.org/packages/Brahma.FSharp/)
[![NuGet Badge](https://buildstats.info/nuget/Brahma.FSharp?includePreReleases=true)](https://www.nuget.org/packages/Brahma.FSharp/)
[![License](https://img.shields.io/badge/License-EPL_1.0-red.svg)](https://opensource.org/licenses/EPL-1.0)

**Brahma.FSharp** provides a way to utilize GPGPU in your F# programs. It is based on F# quotations to OpenCL translation.

## Features
* Utilization of OpenCL for communication with GPU. So, you can work not only with NVIDIA devices but with any device which supports OpenCL (e.g. with AMD or Intel devices).
* Not only primitive types, but also discriminated unions, structs, records are supported.
* Pattern matching, mutable and immutable bindings, nested bindings are supported.
* Custom atomics.
* Fine-grained memory management and kernels compilation process.
* Mailbox processor based interface for communication with devices.

More details are available [here](https://yaccconstructor.github.io/Brahma.FSharp/).

## Installation
Install Brahma.FSharp by running:
```shell
dotnet add package Brahma.FSharp
```

Setup BRAHMA_OCL_PATH environment variable to opencl.dll location if it differs from default.

## Quick Start
```f# script
open Brahma.FSharp

let device = ClDevice.GetFirstAppropriateDevice()
let context = RuntimeContext(device)

let kernel =
    <@
        fun (range: Range1D) (buffer: int clarray) ->
            let gid = range.GlobalID0
            buffer.[gid] <- buffer.[gid] + 1
    @>

opencl {
    use! buffer = ClArray.alloc<int> 1024
    do! runCommand kernel <| fun kernel ->
        kernel
        <| Range1D(1024, 256)
        <| buffer

    return! ClArray.toHost buffer
}
|> ClTask.runSync context
```

## Contributing
Contributions, issues and feature requests are welcome.
Feel free to check [issues](https://github.com/YaccConstructor/Brahma.FSharp/issues) page if you want to contribute.

### Build
Make sure the following **requirements** are installed on your system:
- [dotnet SDK](https://dotnet.microsoft.com/en-us/download/dotnet/7.0) 7.0 or higher
- OpenCL-compatible device and respective OpenCL driver

To build and run all tests:

- on Windows
```cmd
build.cmd 
```

- on Linux/macOS
```shell
./build.sh 
```
To find more options look at [MiniScaffold](https://github.com/TheAngryByrd/MiniScaffold). We use it in our project.

### Relese
The release process is automated: NuGet packages publishing process is triggered by tag pushing to any branch.
To release new vesion one should
1. [Add relese notes to CHANGELOG](https://github.com/TheAngryByrd/MiniScaffold/blob/master/Content/Library/README.md#releasing)
2. Run ```./build.sh Release [version]``` (on local machine)

## License
This project licensed under EPL-1.0 License. License text can be found in the [license file](https://github.com/YaccConstructor/Brahma.FSharp/blob/master/LICENSE.md).
 
