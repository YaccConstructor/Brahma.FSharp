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
open FSharpx.Collections
open Brahma.FSharp.OpenCL.Printer
open Microsoft.FSharp.Collections

module Statements =
    let rec private printAssignment (a: Assignment<'lang>) =
        [ Expressions.print a.Name; wordL "="; Expressions.print a.Value ] |> spaceListL

    and private printSpaceModeifier (sm: AddressSpaceQualifier<_>) =
        match sm with
        | Global -> wordL "__global"
        | Local -> wordL "__local"
        | Constant -> wordL "__constant"
        | Private -> wordL "__private"
        | Default -> wordL "__default"

    and private printVarDecl (vd: VarDecl<'lang>) =
        [
            if vd.SpaceModifier.IsSome then
                yield printSpaceModeifier vd.SpaceModifier.Value
            yield Types.print vd.Type
            yield wordL vd.Name
            if vd.Type :? ArrayType<_> then
                yield wordL "[" ^^ wordL (string vd.Type.Size) ^^ wordL "]"
            if vd.Expr.IsSome && not <| vd.IsLocal() then
                yield [ wordL "="; Expressions.print vd.Expr.Value ] |> spaceListL
        ]
        |> spaceListL

    and private printVar (v: Variable<'lang>) = wordL v.Name

    and private printStmtBlock (sb: StatementBlock<'lang>) =
        sb.Statements
        |> ResizeArray.map (print false)
        |> List.ofSeq
        |> aboveListL
        |> braceL

    and private printIf (if': IfThenElse<_>) =
        let cond = Expressions.print if'.Condition |> bracketL
        let then' = print true if'.Then

        let else' =
            match if'.Else with
            | Some x -> print true x
            | None -> wordL ""

        [
            yield wordL "if" ++ cond
            yield then'
            if if'.Else.IsSome then
                yield aboveL (wordL "else") else'
        ]
        |> aboveListL

    and private printForInteger (for': ForIntegerLoop<_>) =
        let cond = Expressions.print for'.Condition
        let i = print true for'.Var
        let cModif = print true for'.CountModifier
        let body = print true for'.Body

        let header = [ i; cond; cModif ] |> sepListL (wordL ";") |> bracketL

        [ yield wordL "for" ++ header; yield body ] |> aboveListL

    and printWhileLoop (wl: WhileLoop<_>) =
        let cond = Expressions.print wl.Condition |> bracketL
        let body = print true wl.WhileBlock

        [ yield wordL "while" ++ cond; yield body ] |> aboveListL

    and printFunCall (fc: FunCall<_>) =
        let args = fc.Args |> List.map Expressions.print |> commaListL |> bracketL

        wordL fc.Name ++ args

    and printBarrier (b: Barrier<_>) =
        match b.MemFence with
        | MemFence.Local -> wordL "barrier(CLK_LOCAL_MEM_FENCE)"
        | MemFence.Global -> wordL "barrier(CLK_GLOBAL_MEM_FENCE)"
        | Both -> wordL "barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE)"

    and printReturn (r: Return<_>) = wordL "return" ++ Expressions.print r.Expression

    and printFieldSet (fs: FieldSet<_>) =
        let host = Expressions.print fs.Host
        let fld = wordL fs.Field
        let val' = Expressions.print fs.Val

        [ host |> bracketL; wordL "."; fld; wordL "="; val' ] |> spaceListL

    and print isToplevel (stmt: Statement<'lang>) =
        let res =
            match stmt with
            | :? StatementBlock<'lang> as sb -> printStmtBlock sb
            | :? VarDecl<'lang> as vd -> printVarDecl vd
            | :? Assignment<'lang> as a -> printAssignment a
            | :? IfThenElse<'lang> as ite -> printIf ite
            | :? ForIntegerLoop<'lang> as _for -> printForInteger _for
            | :? WhileLoop<'lang> as wl -> printWhileLoop wl
            | :? FunCall<'lang> as fc -> printFunCall fc
            | :? Barrier<'lang> as b -> printBarrier b
            | :? FieldSet<'lang> as fs -> printFieldSet fs
            | :? Return<'lang> as r -> printReturn r
            //| :? Variable<'lang> as v -> printVar v
            | :? Expression<'lang> as e -> Expressions.print e
            | _ -> failwithf $"Printer. Unsupported statement: {stmt}"

        if isToplevel then res else res ++ wordL ";"
