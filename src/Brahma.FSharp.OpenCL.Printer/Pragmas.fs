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

module Brahma.FSharp.OpenCL.Printer.Pragmas

open Brahma.FSharp.OpenCL.AST
open Microsoft.FSharp.Text.StructuredFormat
open Microsoft.FSharp.Text.StructuredFormat.LayoutOps
open Brahma.FSharp.OpenCL.Printer

let Print (clp:CLPragma<_>) =
    match clp.Type with
    | CLGlobalInt32BaseAtomics ->
        [
            "#pragma OPENCL EXTENSION cl_khr_global_int32_base_atomics : enable" |> wordL            
        ] |> aboveListL
    | CLLocalInt32BaseAtomics ->
        [
            "#pragma OPENCL EXTENSION cl_khr_local_int32_base_atomics : enable" |> wordL            
        ] |> aboveListL
    | CLFP64 -> 
        [
            "#pragma OPENCL EXTENSION cl_khr_fp64 : enable" |> wordL            
        ] |> aboveListL