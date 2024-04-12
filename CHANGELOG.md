# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [3.0.0-alpha1.2] - 2024-04-12

### Fixed
- Raw float passing. (Issue #157)
- Channel synchronization.

### Added
- MailboxProcessor replaced with custom agent on System.Threading.Channels.

## [3.0.0-alpha1.1] - 2024-04-09

### Added
- MailboxProcessor replaced with custom agent on System.Threading.Channels.

## [2.0.5] - 2023-02-10

### Added
- Typed device extensions.

## [2.0.4] - 2023-01-13

### Added
- LocalMemSize and GlobalMemSize Device properties.

### Fixed
- Issue #145

## [2.0.3] - 2023-01-04

### Fixed
- All dependencies updated

## [2.0.2] - 2022-12-28

### Added
- .NET 7 is targeted

### Fixed
- DUs are not blittable (.NET 7)

## [2.0.1] - 2022-08-05

### Fixed
- Multiple local values in atomic operation

## [2.0.0] - 2022-07-05

### Fixed
- Blittable types transfer
- Performance issues
- Pointers to private variables are explicitly private
- Performance of blittable types
- Performance of kernels creation
- Issue #135
- Native opencl library linking
- Atomics
- ClArray memory management
- API for buffers manipulation
- Boolean type support (issue ##116, https://github.com/YaccConstructor/Brahma.FSharp/issues/116)
- Nested functions
- Complex let bindings
- ToHost behaviour on non-gpu arrays
- printf/printfn without arguments
- Boolean binary operators
- Transfer arrays of boolean
- Local memory semantic. It is forbidden to initialize variables in the local memory.

- Blittable types transfer
- Performance issues
- Pointers to private variables are explicitly private
- Performance of blittable types
- Performance of kernels creation
- Issue #135
- Native opencl library linking
- Atomics
- ClArray memory management
- API for buffers manipulation
- Boolean type support (issue ##116, https://github.com/YaccConstructor/Brahma.FSharp/issues/116)
- Nested functions
- Complex let bindings
- ToHost behaviour on non-gpu arrays
- printf/printfn without arguments
- Boolean binary operators
- Transfer arrays of boolean
- Local memory semantic. It is forbidden to initialize variables in the local memory.

### Added
- New abstraction for OpenCL device
- Getting workGroupSize inside kernels
- Discriminated unions inside kernel functions
- Stepped and non-integer loops
- ```ClCell``` support
- support of following types
  - ```Tuple``` and ```ValueTuple```
  - Records including generic records
- Workflow builder for OpenCL computations
- New mailbox processor based API
- Targeting .net 5.0
- Kernel compilation caching
- Mutable variables in closures
- support of printf call inside kernel code
- While and for loops in workflow builders
- Basic workflow builders for designing computations

- New abstraction for OpenCL device
- Getting workGroupSize inside kernels
- Discriminated unions inside kernel functions
- Stepped and non-integer loops
- ```ClCell``` support
- support of following types
  - ```Tuple``` and ```ValueTuple```
  - Records including generic records
- Workflow builder for OpenCL computations
- New mailbox processor based API
- Targeting .net 5.0
- Kernel compilation caching
- Mutable variables in closures
- support of printf call inside kernel code
- While and for loops in workflow builders
- Basic workflow builders for designing computations

## [2.0.0-alpha9.6] - 2022-06-03

### Fixed
- Blittable types transfer
- Performance issues

## [2.0.0-alpha9.5] - 2022-05-07

### Added
- New abstraction for OpenCL device
- Getting workGroupSize inside kernels

### Fixed
- Pointers to private variables are explicitly private
- Performance of blittable types
- Performance of kernels creation

## [2.0.0-alpha9.4] - 2022-04-08

### Fixed
- Issue #135

## [2.0.0-alpha9.3] - 2022-03-24

### Added
- Discriminated unions inside kernel functions
- Stepped and non-integer loops

### Fixed
- Native opencl library linking

## [2.0.0-alpha9.2] - 2021-10-16

### Added
- ```ClCell``` support
- support of following types
  - ```Tuple``` and ```ValueTuple```
  - Records including generic records

### Fixed
- Atomics

## [2.0.0-alpha9.1] - 2021-10-16

### Fixed
- ClArray memory management
- API for buffers manipulation

## [2.0.0-alpha9] - 2021-10-15

### Added
- Workflow builder for OpenCL computations

### Fixed
- Boolean type support (issue ##116, https://github.com/YaccConstructor/Brahma.FSharp/issues/116)

## [2.0.0-alpha8] - 2021-09-27

### Added
- New mailbox processor based API
- Targeting .net 5.0

## [2.0.0-alpha7.1] - 2021-07-18

### Added
- Kernel compilation caching

## [2.0.0-alpha7] - 2021-05-19

### Fixed
- Nested functions
- Complex let bindings

### Added
- Mutable variables in closures

## [2.0.0-alpha6.2] - 2021-05-19

### Fixed
- ToHost behaviour on non-gpu arrays

## [2.0.0-alpha6.1] - 2021-03-22

### Fixed
- printf/printfn without arguments

## [2.0.0-alpha6] - 2021-03-22

### Added
- support of printf call inside kernel code

## [2.0.0-alpha5] - 2021-01-27

### Fixed
- Boolean binary operators
- Transfer arrays of boolean

## [2.0.0-alpha4] - 2020-12-27

### Added
- While and for loops in workflow builders

## [2.0.0-alpha3]

### Fixed
- Local memory semantic. It is forbidden to initialize variables in the local memory.

### Added
- Basic workflow builders for designing computations

## [2.0.0-alpha2]

- Atomic functions are polimorphic
- Function for allocation in local memory is revised
- Type provider is removed
- NetStandard 2.1 and .NET 4.6.1 are targeted

## [1.1.5]

- Constant array translation improved.

## [1.1.4]

- Fix translation of access to local ids of 2D.

## [1.1.3]

- Fix OpenCL compiller options.

## [1.1.2]

- More diagnostic information in kernel compiler

## [1.1.1]

- PDBs added
- Autoinstallation of OpenCL.Net.dll.config

## [1.1.0]

- Basic support of structs and tuples.
- OpenCL type provider. Strongly typed kernels from OpenCL code are available in F#.
- Documentation updated.
- More examples added.

## [1.1.0-alpha4]

- Basic support of structs and tuples.

## [1.1.0-alpha3]

- Clean references.

## [1.1.0-alpha2]

- Fix references.

## [1.1.0-alpha1]

- OpenCL type provider. Strongly typed kernels from OpenCL code are available in F#.

## [1.0.1]

- Fix Float type translation

## [1.0.0]

- FSharp.Core form NuGet
- .NET 4.5

[Unreleased]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v3.0.0-alpha1.2...HEAD
[3.0.0-alpha1.2]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v2.0.5...v3.0.0-alpha1.2
[3.0.0-alpha1.1]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v2.0.5...v3.0.0-alpha1.1
[2.0.5]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v2.0.4...v2.0.5
[2.0.4]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v2.0.3...v2.0.4
[2.0.3]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v2.0.2...v2.0.3
[2.0.2]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v2.0.1...v2.0.2
[2.0.1]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v2.0.0...v2.0.1
[2.0.0]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v1.1.5...v2.0.0
[2.0.0-alpha9.6]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v1.1.5...v2.0.0-alpha9.6
[2.0.0-alpha9.5]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v1.1.5...v2.0.0-alpha9.5
[2.0.0-alpha9.4]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v1.1.5...v2.0.0-alpha9.4
[2.0.0-alpha9.3]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v1.1.5...v2.0.0-alpha9.3
[2.0.0-alpha9.2]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v1.1.5...v2.0.0-alpha9.2
[2.0.0-alpha9.1]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v1.1.5...v2.0.0-alpha9.1
[2.0.0-alpha9]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v1.1.5...v2.0.0-alpha9
[2.0.0-alpha8]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v1.1.5...v2.0.0-alpha8
[2.0.0-alpha7.1]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v1.1.5...v2.0.0-alpha7.1
[2.0.0-alpha7]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v1.1.5...v2.0.0-alpha7
[2.0.0-alpha6.2]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v1.1.5...v2.0.0-alpha6.2
[2.0.0-alpha6.1]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v1.1.5...v2.0.0-alpha6.1
[2.0.0-alpha6]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v1.1.5...v2.0.0-alpha6
[2.0.0-alpha5]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v1.1.5...v2.0.0-alpha5
[2.0.0-alpha4]: https://github.com/YaccConstructor/Brahma.FSharp/compare/v1.1.5...v2.0.0-alpha4
