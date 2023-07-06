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

namespace Brahma.FSharp.OpenCL.Printer

open Brahma.FSharp.OpenCL.AST
open Microsoft.FSharp.Text.StructuredFormat.LayoutOps
open Brahma.FSharp.OpenCL.Printer

module Expressions =
    let private printConst (c: Const<'lang>) =
        match c.Type with
        | :? PrimitiveType<'lang> as pt ->
            match pt.Type with
            | Bool
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
            | Half -> wordL c.Val
            | Void -> wordL ""
            | ConstStringLiteral -> wordL <| sprintf "\"%s\"" c.Val
            | TypeName tname -> failwithf "Printer. Unsupported const with type: %A" tname
        | :? RefType<'lang> as rt -> wordL c.Val
        | :? ArrayType<'lang> as rt -> wordL c.Val
        | c -> failwithf "Printer. Unsupported const with type: %A" c

    let private printVar (varible: Variable<'lang>) = wordL varible.Name

    let rec private printItem (itm: Item<'lang>) =
        (print itm.Arr) ++ squareBracketL (print itm.Idx)

    and private printIndirectionOp (deref: IndirectionOp<'lang>) =
        wordL "*" ++ (print deref.Expr |> bracketL)

    and private printBop (op: BOp<'lang>) =
        match op with
        | Plus -> "+"
        | Minus -> "-"
        | Mult -> "*"
        | Div -> "/"
        | Pow -> "+"
        | BitAnd -> "&"
        | BitOr -> "|"
        | BitXor -> "^"
        | LeftShift -> "<<"
        | RightShift -> ">>"
        | And -> "&&"
        | Or -> "||"
        | Less -> "<"
        | LessEQ -> "<="
        | Great -> ">"
        | GreatEQ -> ">="
        | EQ -> "=="
        | NEQ -> "!="
        | Remainder -> "%"
        |> wordL

    and private printBinop (binop: Binop<'lang>) =
        let l = print binop.Left
        let r = print binop.Right
        let op = printBop binop.Op

        [ l
          op
          r ]
        |> spaceListL
        |> bracketL

    and private printProperty (prop: Property<'lang>) =
        match prop.Property with
        | PropertyType.Var v -> printVar v
        | PropertyType.Item i -> printItem i
        | PropertyType.VarReference p -> printIndirectionOp p

    and private printFunCall (fc: FunCall<'lang>) =
        let isVoidArg (expr: Expression<_>) =
            match expr with
            | :? Const<_> as c ->
                match c.Type with
                | :? PrimitiveType<_> as pt -> pt.Type = Void
                | _ -> false
            | _ -> false

        // TODO: move filtration into translator
        let argsLayout =
            fc.Args
            |> List.filter (not << isVoidArg)
            |> List.map print
            |> commaListL
            |> bracketL

        wordL fc.Name ++ argsLayout

    and private printUnOp (uo: Unop<'lang>) =
        match uo.Op with
        | UOp.Minus -> wordL "-" ++ print uo.Expr |> bracketL
        | UOp.Not -> wordL "!" ++ print uo.Expr |> bracketL
        | UOp.Incr -> print uo.Expr ++ wordL "++"
        | UOp.Decr -> print uo.Expr ++ wordL "--"
        | UOp.BitNegation -> wordL "~" ++ print uo.Expr |> bracketL

    and private printCast (c: Cast<'lang>) =
        let t = Types.print c.Type
        let expr = print c.Expr
        (t |> bracketL) ++ expr

    and private printPointer (p: Ptr<'lang>) =
        let expr = print p.Expr
        wordL "&" ^^ expr

    and private printArrayInitializer (ai: ArrayInitializer<'lang>) =
        match ai with
        | :? ZeroArray<_> as za -> wordL "{0}"
        | other -> failwithf "Printer. Unsupported array initializer: %A" other

    and private getZeros x =
        let mutable string = "{0"

        for i in 1 .. (x - 1) do
            string <- string + ",0"

        string <- string + "}"
        string

    and printNewStruct (newStruct: NewStruct<_>) =
        let args = List.map print newStruct.ConstructorArgs |> commaListL

        match newStruct.Struct with
        | :? StructInplaceType<_> ->
            [ wordL "{"
              args
              wordL "}" ]
            |> spaceListL
        | _ ->
            let t = Types.print newStruct.Struct

            [ t |> bracketL
              wordL "{"
              args
              wordL "}" ]
            |> spaceListL

    and printNewUnion (newUnion: NewUnion<_>) =
        let arg = print newUnion.ConstructorArg

        [ wordL "{"
          wordL <| "." + newUnion.ConstructorArgName
          wordL "="
          arg
          wordL "}" ]
        |> spaceListL

    and printFfieldGet (fg: FieldGet<_>) =
        let host = print fg.Host
        let fld = wordL fg.Field

        [ host |> bracketL
          wordL "."
          fld ]
        |> spaceListL

    and print (expr: Expression<'lang>) =
        match expr with
        | :? Const<'lang> as c -> printConst c
        | :? Variable<'lang> as v -> printVar v
        | :? Item<'lang> as itm -> printItem itm
        | :? Property<'lang> as prop -> printProperty prop
        | :? Binop<'lang> as binop -> printBinop binop
        | :? FunCall<'lang> as fc -> printFunCall fc
        | :? Unop<'lang> as uo -> printUnOp uo
        | :? Cast<'lang> as c -> printCast c
        | :? Ptr<'lang> as p -> printPointer p
        | :? ArrayInitializer<'lang> as ai -> printArrayInitializer ai
        | :? NewStruct<'lang> as ns -> printNewStruct ns
        | :? NewUnion<'lang> as nu -> printNewUnion nu
        | :? FieldGet<'lang> as fg -> printFfieldGet fg
        | :? IndirectionOp<'lang> as ip -> printIndirectionOp ip
        | c -> failwithf "Printer. Unsupported expression: %A" c
