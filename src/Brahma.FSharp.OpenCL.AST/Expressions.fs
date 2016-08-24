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

[<AbstractClass>]
type Expression<'lang>()=
    inherit Statement<'lang>()

type Const<'lang>(_type:Type<'lang>,_val:string) =
    inherit Expression<'lang>()
    override this.Children = []
    member this.Type = _type
    member this.Val = _val

type Variable<'lang>(name:string) =
    inherit Expression<'lang>()
    override this.Children = []
    member this.Name = name

type FunCall<'lang>(funName:string, args:List<Expression<'lang>>) = 
    inherit Expression<'lang>()
    override this.Children = []
    member this.Name = funName
    member this.Args = args

type Item<'lang>(arr:Expression<'lang>,idx:Expression<'lang>) = 
    inherit Expression<'lang>()
    override this.Children = []
    member this.Arr = arr
    member this.Idx = idx

[<RequireQualifiedAccess>]
type PropertyType<'lang>=
    | Var of Variable<'lang>
    | Item of Item<'lang>

type Property<'lang>(property:PropertyType<'lang>) =
    inherit Expression<'lang>()
    override this.Children = []
    member this.Property = property

type BOp<'lang> =
     | Plus
     | Minus
     | Mult
     | Div
     | Pow
     | BitAnd
     | BitOr
     | LeftShift
     | RightShift
     | And
     | Or
     | Less
     | LessEQ
     | Great
     | GreatEQ
     | EQ
     | NEQ
     | Remainder

type Binop<'lang>(op:BOp<'lang>,l:Expression<'lang>,r:Expression<'lang>) = 
    inherit Expression<'lang>()
    override this.Children = []
    member this.Left = l
    member this.Right = r
    member this.Op = op

[<RequireQualifiedAccess>]
type UOp<'lang> =
    | Minus
    | Not
    | Incr
    | Decr

type Unop<'lang>(op:UOp<'lang>,expr:Expression<'lang>) = 
    inherit Expression<'lang>()
    override this.Children = []
    member this.Expr = expr
    member this.Op = op

type Cast<'lang>(expr:Expression<'lang>,_type:Type<'lang>)=
    inherit Expression<'lang>()
    override this.Children = []
    member this.Expr = expr
    member this.Type = _type

type Pointer<'lang>(expr:Expression<'lang>)=
    inherit Expression<'lang>()
    override this.Children = []
    member this.Expr = expr    

[<AbstractClass>]
type ArrayInitializer<'lang>() =
    inherit Expression<'lang>()
    override this.Children = []
    abstract Length : int

type ZeroArray<'lang>(length:int) =
    inherit ArrayInitializer<'lang>()
    override this.Length = length

type NewStruct<'lang>(structInfo:Struct<'lang>, cArgs:List<Expression<'lang>>) =
    inherit Expression<'lang>()
    override this.Children = structInfo :> _ :: List.ofSeq (Seq.cast<_> cArgs)
    member this.Struct = structInfo
    member this.ConstructorArgs = cArgs

type FieldGet<'lang>(host:Expression<'lang>, field:string) =
    inherit Expression<'lang>()
    override this.Children = []
    member this.Host = host
    member this.Field = field
    
