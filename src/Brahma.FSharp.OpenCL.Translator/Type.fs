// Copyright (c) 2012, 2013 Semyon Grigorev <rsdpisuy@gmail.com>
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

module Brahma.FSharp.OpenCL.Translator.Type

open Brahma.FSharp.OpenCL.AST
open System.Reflection
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Quotations

let rec Translate (_type:System.Type) isKernelArg size (context:TargetContext<_,_>) : Type<Lang> =
    let rec go (str:string) =
        let low = str.ToLowerInvariant()
        match low with
        | "int"| "int32" -> PrimitiveType<Lang>(Int) :> Type<Lang>
        | "int16" -> PrimitiveType<Lang>(Short) :> Type<Lang>
        | "uint16" -> PrimitiveType<Lang>(UShort) :> Type<Lang>
        | "uint32" -> PrimitiveType<Lang>(UInt) :> Type<Lang>
        | "float32" | "single"-> PrimitiveType<Lang>(Float) :> Type<Lang>
        | "byte" -> PrimitiveType<Lang>(UChar) :> Type<Lang>
        | "int64" -> PrimitiveType<Lang>(Long) :> Type<Lang>
        | "uint64" -> PrimitiveType<Lang>(ULong) :> Type<Lang>
        | "boolean" -> PrimitiveType<Lang>(Int) :> Type<Lang>
        | "float" | "double" -> 
            context.Flags.enableFP64 <- true
            PrimitiveType<Lang>(Double) :> Type<Lang>        
        | "unit" -> PrimitiveType<Lang>(Void) :> Type<Lang>
        | t when t.EndsWith "[]" ->
            let baseT = t.Substring(0,t.Length-2)
            if isKernelArg 
            then RefType<_>(go baseT) :> Type<Lang>
            else ArrayType<_>(go baseT, size |> Option.get) :> Type<Lang>
        | s when s.StartsWith "fsharpref" ->
            go (_type.GetGenericArguments().[0].Name)
        | f when f.StartsWith "fsharpfunc" ->
//            go (_type.GetGenericArguments().[1].Name)
            Translate (_type.GetGenericArguments().[1]) isKernelArg size context
        | x when context.UserDefinedTypes.Exists(fun t -> t.Name.ToLowerInvariant() = x)
            -> 
                let decl =
                    if context.UserDefinedTypesOpenCLDeclaration.ContainsKey x
                    then Some context.UserDefinedTypesOpenCLDeclaration.[x]
                    else None 
                StructType(decl) :> Type<Lang>
        | x -> "Unsuported kernel type: " + x |> failwith 
    _type.Name
    |> go


let TransleteStructDecls structs (targetContext:TargetContext<_,_>) =    
    let translateStruct (t:System.Type) =
        let name = t.Name
        let fields = [ for f in 
                            t.GetProperties (BindingFlags.Public ||| BindingFlags.Instance) ->
                        new StructField<_> (f.Name, Translate f.PropertyType true None targetContext)]
                     @
                     [ for f in 
                            t.GetFields(BindingFlags.Public ||| BindingFlags.Instance) ->
                        new StructField<_> (f.Name, Translate f.FieldType true None targetContext)]
                                             
        new Struct<_>(name, fields)

    let translated = 
        do targetContext.UserDefinedTypes.AddRange(structs)
        structs
        |> List.ofSeq
        |> List.map 
            (fun t -> 
                let r = translateStruct t
                targetContext.UserDefinedTypesOpenCLDeclaration.Add(t.Name.ToLowerInvariant(),r)
                r)
    translated