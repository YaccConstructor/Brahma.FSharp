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

module Brahma.FSharp.OpenCL.Printer.Statements

open Brahma.FSharp.OpenCL.AST
open Microsoft.FSharp.Text.StructuredFormat
open Microsoft.FSharp.Text.StructuredFormat.LayoutOps
open Brahma.FSharp.OpenCL.Printer
open Microsoft.FSharp.Collections

let rec private printAssignment (a:Assignment<'lang>) =
    [Expressions.Print a.Name; wordL "="; Expressions.Print a.Value] |> spaceListL

and private printVarDecl (vd:VarDecl<'lang>) =
    [ if vd.IsLocal then yield wordL "__local"
    ; yield Types.Print vd.Type
    ; yield wordL vd.Name
    ; if vd.Type :? ArrayType<_> then yield wordL "["  ^^ wordL (string vd.Type.Size)  ^^ wordL "]"
    ; if vd.Expr.IsSome && not vd.IsLocal then yield [wordL "="; Expressions.Print vd.Expr.Value] |> spaceListL
    ] |> spaceListL

and private printVar (v: Variable<'lang>) =
    wordL v.Name

and private printStmtBlock (sb:StatementBlock<'lang>) =
    sb.Statements |> ResizeArray.map (Print false) |> List.ofSeq |> aboveListL |> braceL

and private printIf (_if:IfThenElse<_>) =
    let cond = Expressions.Print _if.Condition |> bracketL
    let _then = Print true _if.Then
    let _else = 
        match _if.Else with
        | Some x -> Print true x
        | None -> wordL ""
    [ yield wordL "if" ++ cond 
    ; yield _then
    ; if _if.Else.IsSome then yield aboveL (wordL "else") _else]
    |> aboveListL

and private printForInteger (_for:ForIntegerLoop<_>) =
    let cond = Expressions.Print _for.Condition 
    let i = Print true _for.Var
    let cModif = Expressions.Print _for.CountModifier
    let body = Print true _for.Body
    let header = [i; cond; cModif] |> sepListL (wordL ";") |> bracketL
    [ yield wordL "for" ++ header
    ; yield body]
    |> aboveListL
    
and printWhileLoop (wl:WhileLoop<_>) =
    let cond = Expressions.Print wl.Condition |> bracketL
    let body = Print true wl.WhileBlock
    [ yield wordL "while" ++ cond; 
    yield body]
    |> aboveListL

and printFunCall (fc:FunCall<_>) =
    let args = fc.Args |> List.map Expressions.Print |> commaListL |> bracketL    
    wordL fc.Name ++ args 
    
and printBarrier (b:Barrier<_>) =
    wordL "barrier(CLK_LOCAL_MEM_FENCE)"   

and printRetun (r:Return<_>) =
    wordL "return" ++ Expressions.Print r.Expression
    
and printFieldSet (fs:FieldSet<_>) =
    let host = Expressions.Print fs.Host
    let fld = wordL fs.Field
    let _val = Expressions.Print fs.Val
    [
        host |> bracketL
        wordL "."
        fld
        wordL "="
        _val
    ] |> spaceListL

and Print isToplevel (stmt:Statement<'lang>) =
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
        | :? Return<'lang> as r -> printRetun r
        //| :? Variable<'lang> as v -> printVar v
        | t -> failwithf "Printer. Unsupported statement: %A" t
    if isToplevel
    then res
    else res ++ wordL ";"