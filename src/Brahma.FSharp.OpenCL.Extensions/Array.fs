// Copyright (c) 2013 Semyon Grigorev <rsdpisuy@gmail.com>
// All rights reserved.
// 
// The contents of this file are made available under the terms of the
// Eclipse Public License v1.0 (the "License") which accompanies this
// distribution, and is available at the following URL:
// http://www.opensource.org/licenses/eclipse-1.0.php
// 
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
// the specific language governing rights and limitations under the License.
// 
// By using this software in any fashion, you are agreeing to be bound by the
// terms of the License.

[<AutoOpen>]
module Brahma.FSharp.OpenCL.Extensions

open System.Runtime.InteropServices
open OpenCL.Net
open Brahma.OpenCL

type ``[]``<'t> with
    /// Write all data from array a into corresponded buffer.
    member this.ToGpu(provider:ComputeProvider) =
        new Commands.WriteBuffer<'t>(provider.AutoconfiguredBuffers.[this], Marshal.SizeOf(typeof<'t>), true, 0, this.Length,this)
    /// Write data from array data into corresponded buffer. Transferred data length is min of a.Length and hostArray.Lenght.
    member this.ToGpu(provider:ComputeProvider, hostArray:array<'t>) =
        new Commands.WriteBuffer<'t>(provider.AutoconfiguredBuffers.[this], Marshal.SizeOf(typeof<'t>), true, 0, (min hostArray.Length this.Length), hostArray)

    /// Read all data from corresponded buffer into array a.
    member this.ToHost(provider:ComputeProvider) =       
        new Commands.ReadBuffer<'t>(provider.AutoconfiguredBuffers.[this], Marshal.SizeOf(typeof<'t>), true, 0, this.Length, this)
    ///  Read data from corresponded buffer into array hostArray. Transferred data length is min of a.Length and hostArray.Lenght.
    member this.ToHost(provider:ComputeProvider, hostArray:array<'t>) =
        new Commands.ReadBuffer<'t>(provider.AutoconfiguredBuffers.[this], Marshal.SizeOf(typeof<'t>), true, 0, (min hostArray.Length this.Length), hostArray)
    ///  Read data from corresponded buffer into array hostArray. Transferred data length is length.
    member this.ToHost(provider:ComputeProvider, hostArray:array<'t>, length) =
        new Commands.ReadBuffer<'t>(provider.AutoconfiguredBuffers.[this], Marshal.SizeOf(typeof<'t>), true, 0, length, hostArray)