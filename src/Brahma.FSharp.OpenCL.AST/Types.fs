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

namespace Brahma.FSharp.OpenCL.AST

type PTypes<'lang> =
    | Char
    | UChar
    | Short
    | UShort
    | Int
    | UInt
    | Long
    | ULong
    | Float
    | Double
    | Void

[<AbstractClass>]
type Type<'lang>()=
    inherit Node<'lang>()
    abstract Size:int
    //abstract SpaceModifier:SpaceModifier

type PrimitiveType<'lang>(pType:PTypes<'lang>) =
    inherit Type<'lang>()
    override this.Size = 32
    override this.Children = []
    //override this.SpaceModifier = Private
    member this.Type = pType

type ArrayType<'lang>(baseType:Type<'lang>, size:int) =
    inherit Type<'lang>()
    override this.Size = size
    override this.Children = []
    //override this.SpaceModifier = match spaceModeifier with Some x -> x | None -> Private
    member this.BaseType = baseType

[<Struct>]
type StructField<'lang> =
    val FName: string
    val FType: Type<'lang>

    new (fName, fType) = {FName = fName; FType = fType}

type Struct<'lang>(name: string, fields: List<StructField<'lang>>) =
    inherit TopDef<'lang>()
    override this.Children = []
    member this.Fields = fields
    member this.Name = name

type StructType<'lang>(decl)=    
    inherit Type<'lang>()
    member val Declaration : Option<Struct<'lang>> = decl with get, set
    override this.Children = []
    override this.Size = 
        match  this.Declaration with
        | Some decl -> decl.Fields |> List.sumBy (fun f -> f.FType.Size)
        | None -> 0
   

        

type RefType<'lang>(baseType:Type<'lang>) =
    inherit Type<'lang>()
    override this.Size = baseType.Size
    override this.Children = []
    //override this.SpaceModifier = match spaceModeifier with Some x -> x | None -> Global
    member this.BaseType = baseType