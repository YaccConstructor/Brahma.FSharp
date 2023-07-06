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

namespace Brahma.FSharp.OpenCL.Translator

open System.Reflection
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.AST
open Microsoft.FSharp.Collections
open FSharpx.Collections
open Brahma.FSharp.OpenCL.Translator.QuotationTransformers
open FSharp.Quotations.Evaluator
open Brahma.FSharp

// Translations restricts the generic parameter of the AST nodes to the type Lang
#nowarn "64"

[<AutoOpen>]
module private BodyPatterns =
    let (|VarName|_|) (str: string) (var': Var) =
        match var'.Name with
        | tName when tName = str -> Some VarName
        | _ -> None

    let (|Lower|_|) (expected: string) (str: string) =
        match str with
        | _ when expected.ToLowerInvariant() = str.ToLowerInvariant() -> Some Lower
        | _ -> None

    let (|ForLoopWithStep|_|) =
        function
        | Patterns.Let(VarName "inputSequence",
                       DerivedPatterns.SpecificCall <@ (.. ..) @> (_, _, [ start; step; finish ]),
                       Patterns.Let(VarName "enumerator",
                                    _,
                                    Patterns.TryFinally(Patterns.WhileLoop(_, Patterns.Let(loopVar, _, loopBody)), _))) ->
            Some(loopVar, (start, step, finish), loopBody)
        | _ -> None

    let (|ForLoop|_|) =
        function
        | Patterns.Let(VarName "inputSequence",
                       DerivedPatterns.SpecificCall <@ (..) @> (_, _, [ start; finish ]),
                       Patterns.Let(VarName "enumerator",
                                    _,
                                    Patterns.TryFinally(Patterns.WhileLoop(_, Patterns.Let(loopVar, _, loopBody)), _))) ->
            Some(loopVar, (start, finish), loopBody)
        | _ -> None

module rec Body =
    // new var scope
    let private clearContext (targetContext: TranslationContext<'a, 'b>) =
        { targetContext with VarDecls = ResizeArray() }

    let toStb (s: Node<_>) =
        translation {
            match s with
            | :? StatementBlock<_> as s -> return s
            | x -> return StatementBlock <| ResizeArray [ x :?> Statement<_> ]
        }

    let private itemHelper exprs hostVar =
        translation {
            let! idx =
                translation {
                    match exprs with
                    | hd :: _ -> return! translateAsExpr hd
                    | [] -> return raise <| InvalidKernelException("Array index missed!")
                }

            return idx, hostVar
        }

    let private translateBinding (var: Var) newName (expr: Expr) =
        translation {
            let! body = translateCond (*TranslateAsExpr*) expr

            let! varType =
                translation {
                    match (body: Expression<_>) with
                    | :? Const<_> as c -> return c.Type
                    | :? ArrayInitializer<_> as ai ->
                        return!
                            Type.translate var.Type
                            |> State.using (fun ctx -> { ctx with ArrayKind = CArrayDecl ai.Length })
                    | _ -> return! Type.translate var.Type
                }

            let varDecl = VarDecl(varType, newName, Some body)

            if varType :? RefType<Lang> then
                varDecl.SpaceModifier <- Some AddressSpaceQualifier.Private

            return varDecl
        }

    let private translateListOfArgs (args: Expr list) =
        args
        |> List.fold
            (fun state arg ->
                translation {
                    let! state = state
                    let! translated = translateCond arg
                    return translated :: state
                })
            (State.return' [])
        |> State.map List.rev

    let private translateCall exprOpt (mInfo: System.Reflection.MethodInfo) args =
        translation {
            let! args = translateListOfArgs args

            match mInfo.Name.ToLowerInvariant() with
            | "op_multiply" -> return Binop(Mult, args.[0], args.[1]) :> Statement<_>
            | "op_addition" -> return Binop(Plus, args.[0], args.[1]) :> Statement<_>
            | "op_division" -> return Binop(Div, args.[0], args.[1]) :> Statement<_>
            | "op_lessthan" -> return Binop(Less, args.[0], args.[1]) :> Statement<_>
            | "op_lessthanorequal" -> return Binop(LessEQ, args.[0], args.[1]) :> Statement<_>
            | "op_greaterthan" -> return Binop(Great, args.[0], args.[1]) :> Statement<_>
            | "op_greaterthanorequal" -> return Binop(GreatEQ, args.[0], args.[1]) :> Statement<_>
            | "op_equality" -> return Binop(EQ, args.[0], args.[1]) :> Statement<_>
            | "op_inequality" -> return Binop(NEQ, args.[0], args.[1]) :> Statement<_>
            | "op_subtraction" -> return Binop(Minus, args.[0], args.[1]) :> Statement<_>
            | "op_unarynegation" -> return Unop(UOp.Minus, args.[0]) :> Statement<_>
            | "op_modulus" -> return Binop(Remainder, args.[0], args.[1]) :> Statement<_>
            | "op_bitwiseand" -> return Binop(BitAnd, args.[0], args.[1]) :> Statement<_>
            | "op_bitwiseor" -> return Binop(BitOr, args.[0], args.[1]) :> Statement<_>
            | "op_exclusiveor" -> return Binop(BitXor, args.[0], args.[1]) :> Statement<_>
            | "op_logicalnot" -> return Unop(UOp.BitNegation, args.[0]) :> Statement<_>
            | "op_leftshift" -> return Binop(LeftShift, args.[0], args.[1]) :> Statement<_>
            | "op_rightshift" -> return Binop(RightShift, args.[0], args.[1]) :> Statement<_>
            | "op_booleanand" ->
                let! flag = State.gets (fun context -> context.TranslatorOptions.UseNativeBooleanType)

                if flag then
                    return Binop(And, args.[0], args.[1]) :> Statement<_>
                else
                    return Binop(BitAnd, args.[0], args.[1]) :> Statement<_>
            | "op_booleanor" ->
                let! flag = State.gets (fun context -> context.TranslatorOptions.UseNativeBooleanType)

                if flag then
                    return Binop(Or, args.[0], args.[1]) :> Statement<_>
                else
                    return Binop(BitOr, args.[0], args.[1]) :> Statement<_>
            | "not" -> return Unop(UOp.Not, args.[0]) :> Statement<_>
            | "atomicadd" ->
                do!
                    State.modify (fun context ->
                        context.Flags.Add EnableAtomic |> ignore
                        context)

                return
                    FunCall(
                        "atom_add",
                        [ args.[0]
                          args.[1] ]
                    )
                    :> Statement<_>
            | "atomicsub" ->
                do!
                    State.modify (fun context ->
                        context.Flags.Add EnableAtomic |> ignore
                        context)

                return
                    FunCall(
                        "atom_sub",
                        [ args.[0]
                          args.[1] ]
                    )
                    :> Statement<_>
            | "atomicxchg" ->
                do!
                    State.modify (fun context ->
                        context.Flags.Add EnableAtomic |> ignore
                        context)

                return
                    FunCall(
                        "atom_xchg",
                        [ args.[0]
                          args.[1] ]
                    )
                    :> Statement<_>
            | "atomicmax" ->
                do!
                    State.modify (fun context ->
                        context.Flags.Add EnableAtomic |> ignore
                        context)

                return
                    FunCall(
                        "atom_max",
                        [ args.[0]
                          args.[1] ]
                    )
                    :> Statement<_>
            | "atomicmin" ->
                do!
                    State.modify (fun context ->
                        context.Flags.Add EnableAtomic |> ignore
                        context)

                return
                    FunCall(
                        "atom_min",
                        [ args.[0]
                          args.[1] ]
                    )
                    :> Statement<_>
            | "atomicinc" ->
                do!
                    State.modify (fun context ->
                        context.Flags.Add EnableAtomic |> ignore
                        context)

                return FunCall("atom_inc", [ args.[0] ]) :> Statement<_>
            | "atomicdec" ->
                do!
                    State.modify (fun context ->
                        context.Flags.Add EnableAtomic |> ignore
                        context)

                return FunCall("atom_dec", [ args.[0] ]) :> Statement<_>
            | "atomiccmpxchg" ->
                do!
                    State.modify (fun context ->
                        context.Flags.Add EnableAtomic |> ignore
                        context)

                return
                    FunCall(
                        "atom_cmpxchg",
                        [ args.[0]
                          args.[1]
                          args.[2] ]
                    )
                    :> Statement<_>
            | "atomicand" ->
                do!
                    State.modify (fun context ->
                        context.Flags.Add EnableAtomic |> ignore
                        context)

                return
                    FunCall(
                        "atom_and",
                        [ args.[0]
                          args.[1] ]
                    )
                    :> Statement<_>
            | "atomicor" ->
                do!
                    State.modify (fun context ->
                        context.Flags.Add EnableAtomic |> ignore
                        context)

                return
                    FunCall(
                        "atom_or",
                        [ args.[0]
                          args.[1] ]
                    )
                    :> Statement<_>
            | "atomicxor" ->
                do!
                    State.modify (fun context ->
                        context.Flags.Add EnableAtomic |> ignore
                        context)

                return
                    FunCall(
                        "atom_xor",
                        [ args.[0]
                          args.[1] ]
                    )
                    :> Statement<_>
            | "todouble" -> return Cast(args.[0], PrimitiveType Float) :> Statement<_>
            | "toint" -> return Cast(args.[0], PrimitiveType Int) :> Statement<_>
            | "toint16" -> return Cast(args.[0], PrimitiveType Short) :> Statement<_>
            | "tosingle" -> return Cast(args.[0], PrimitiveType Float) :> Statement<_>
            | "tobyte" -> return Cast(args.[0], PrimitiveType UChar) :> Statement<_>
            | "touint32" -> return Cast(args.[0], PrimitiveType UInt) :> Statement<_>
            | "touint16" -> return Cast(args.[0], PrimitiveType UShort) :> Statement<_>
            | "toint64" -> return Cast(args.[0], PrimitiveType Long) :> Statement<_>
            | "touint64" -> return Cast(args.[0], PrimitiveType ULong) :> Statement<_>
            | "min"
            | "max"
            | "acos"
            | "asin"
            | "atan"
            | "cos"
            | "cosh"
            | "exp"
            | "floor"
            | "log"
            | "log10"
            | "pow"
            | "sin"
            | "sinh"
            | "sqrt"
            | "tan"
            | "tanh" as fName ->
                if
                    mInfo.DeclaringType.AssemblyQualifiedName.StartsWith("System.Math")
                    || mInfo.DeclaringType.AssemblyQualifiedName.StartsWith("Microsoft.FSharp.Core.Operators")
                then
                    return FunCall(fName, args) :> Statement<_>
                else
                    return
                        raise
                        <| InvalidKernelException
                            $"Seems, that you use math function with name %s{fName} not from System.Math or Microsoft.FSharp.Core.Operators"
            | "abs" as fName ->
                if mInfo.DeclaringType.AssemblyQualifiedName.StartsWith("Microsoft.FSharp.Core.Operators") then
                    return FunCall("fabs", args) :> Statement<_>
                else
                    return
                        raise
                        <| InvalidKernelException
                            $"Seems, that you use math function with name %s{fName} not from System.Math or Microsoft.FSharp.Core.Operators"
            | "powinteger" as fName ->
                if mInfo.DeclaringType.AssemblyQualifiedName.StartsWith("Microsoft.FSharp.Core.Operators") then
                    return FunCall("powr", args) :> Statement<_>
                else
                    return
                        raise
                        <| InvalidKernelException
                            $"Seems, that you use math function with name %s{fName} not from System.Math or Microsoft.FSharp.Core.Operators"
            | "ref" -> return Ptr args.[0] :> Statement<_>
            | "op_dereference" -> return IndirectionOp args.[0] :> Statement<_>
            | "op_colonequals" ->
                return Assignment(Property(PropertyType.VarReference(IndirectionOp args.[0])), args.[1]) :> Statement<_>
            | "setarray" ->
                return Assignment(Property(PropertyType.Item(Item(args.[0], args.[1]))), args.[2]) :> Statement<_>
            | "getarray" -> return Item(args.[0], args.[1]) :> Statement<_>
            | "barrierlocal" -> return Barrier(MemFence.Local) :> Statement<_>
            | "barrierglobal" -> return Barrier(MemFence.Global) :> Statement<_>
            | "barrierfull" -> return Barrier(MemFence.Both) :> Statement<_>
            | "local" ->
                return
                    raise
                    <| InvalidKernelException(
                        "Calling the local function is allowed only at the top level of the let binding"
                    )
            | "arraylocal" ->
                return
                    raise
                    <| InvalidKernelException(
                        "Calling the localArray function is allowed only at the top level of the let binding"
                    )
            | "zerocreate" ->
                let length =
                    match args.[0] with
                    | :? Const<Lang> as c -> int c.Val
                    | other ->
                        raise
                        <| InvalidKernelException $"Calling Array.zeroCreate with a non-const argument: %A{other}"

                return ZeroArray length :> Statement<_>
            | "fst" -> return FieldGet(args.[0], "_1") :> Statement<_>
            | "snd" -> return FieldGet(args.[0], "_2") :> Statement<_>
            | other -> return raise <| InvalidKernelException $"Unsupported call: %s{other}"
        }

    // TODO: Refactoring: Safe pattern matching by expr type.
    let private translateSpecificPropGet expr propName exprs =
        translation {
            let! hostVar = translateAsExpr expr

            match propName with
            | "globalid0i"
            | "globalid0" -> return FunCall("get_global_id", [ Const(PrimitiveType Int, "0") ]) :> Expression<_>
            | "globalid1i"
            | "globalid1" -> return FunCall("get_global_id", [ Const(PrimitiveType Int, "1") ]) :> Expression<_>
            | "globalid2i"
            | "globalid2" -> return FunCall("get_global_id", [ Const(PrimitiveType Int, "2") ]) :> Expression<_>

            | "localid0" -> return FunCall("get_local_id", [ Const(PrimitiveType Int, "0") ]) :> Expression<_>
            | "localid1" -> return FunCall("get_local_id", [ Const(PrimitiveType Int, "1") ]) :> Expression<_>
            | "localid2" -> return FunCall("get_local_id", [ Const(PrimitiveType Int, "2") ]) :> Expression<_>

            | "item" ->
                let! (idx, hVar) = itemHelper exprs hostVar
                return Item(hVar, idx) :> Expression<_>

            // TODO rewrite to active pattern
            | "value" when
                match expr with
                | Patterns.Var v -> Some v
                | _ -> None
                |> Option.exists (fun v -> v.Type.Name.ToLower().StartsWith ClCell_)
                ->

                let! (idx, hVar) = itemHelper [ Expr.Value 0 ] hostVar
                return Item(hVar, idx) :> Expression<_>

            | _ -> return raise <| InvalidKernelException $"Unsupported property in kernel: %A{propName}"
        }

    let private translatePropGet (exprOpt: Expr Option) (propInfo: PropertyInfo) exprs =
        translation {
            let propName = propInfo.Name.ToLowerInvariant()

            match exprOpt with
            | Some expr ->
                match! State.gets (fun context -> context.CStructDecls.Keys |> Seq.contains expr.Type) with
                | true ->
                    match!
                        State.gets (fun context -> not <| context.CStructDecls.[expr.Type] :? DiscriminatedUnionType<_>)
                    with
                    | true -> return! translateStructFieldGet expr propInfo.Name
                    | false -> return! translateUnionFieldGet expr propInfo
                | false -> return! translateSpecificPropGet expr propName exprs

            | None ->
                match propName with
                | Lower(nameof Anchors._localID0) ->
                    return FunCall("get_local_id", [ Const(PrimitiveType Int, "0") ]) :> Expression<_>

                | Lower(nameof Anchors._globalSize0) ->
                    return FunCall("get_global_size", [ Const(PrimitiveType Int, "0") ]) :> Expression<_>
                | Lower(nameof Anchors._globalSize1) ->
                    return FunCall("get_global_size", [ Const(PrimitiveType Int, "1") ]) :> Expression<_>
                | Lower(nameof Anchors._globalSize2) ->
                    return FunCall("get_global_size", [ Const(PrimitiveType Int, "2") ]) :> Expression<_>

                | Lower(nameof Anchors._localSize0) ->
                    return FunCall("get_local_size", [ Const(PrimitiveType Int, "0") ]) :> Expression<_>
                | Lower(nameof Anchors._localSize1) ->
                    return FunCall("get_local_size", [ Const(PrimitiveType Int, "1") ]) :> Expression<_>
                | Lower(nameof Anchors._localSize2) ->
                    return FunCall("get_local_size", [ Const(PrimitiveType Int, "2") ]) :> Expression<_>

                | _ ->
                    return
                        raise
                        <| InvalidKernelException $"Unsupported static property get in kernel: %A{propName}"
        }

    let private translatePropSet exprOpt (propInfo: System.Reflection.PropertyInfo) exprs newVal =
        translation {
            // TODO: Safe pattern matching (item) by expr type
            let propName = propInfo.Name.ToLowerInvariant()

            match exprOpt with
            | Some expr ->
                let! hostVar = translateAsExpr expr
                let! newVal' = translateAsExpr newVal

                return!
                    translation {
                        match propInfo.Name.ToLowerInvariant() with
                        | "item" ->
                            let! (idx, hVar) = itemHelper exprs hostVar
                            let item = Item(hVar, idx)

                            return Assignment(Property(PropertyType.Item item), newVal') :> Statement<_>
                        // TODO rewrite to active pattern
                        | "value" when
                            match expr with
                            | Patterns.Var v -> Some v
                            | _ -> None
                            |> Option.exists (fun v -> v.Type.Name.ToLower().StartsWith ClCell_)
                            ->

                            let! (idx, hVar) = itemHelper [ Expr.Value 0 ] hostVar
                            let item = Item(hVar, idx)

                            return Assignment(Property(PropertyType.Item item), newVal') :> Statement<_>
                        | _ ->
                            let! translated = translateFieldSet expr propInfo.Name newVal
                            return translated :> Statement<_>
                    }
            | None ->
                return
                    raise
                    <| InvalidKernelException $"Unsupported static property set in kernel: %A{propName}"
        }

    let translateAsExpr expr =
        translation {
            let! (translated: Node<_>) = translate expr
            return translated :?> Expression<_>
        }

    let translateVar (var: Var) =
        translation {
            match! State.gets (fun context -> context.Namer.GetCLVarName var.Name) with
            | Some varName -> return Variable varName
            | None ->
                return
                    raise
                    <| InvalidKernelException
                        $"Seems, that you try to use variable with name %A{var.Name}, that declared out of quotation. \
                    Please, pass it as quoted function's parameter."
        }

    let translateValue (value: obj) (sType: System.Type) =
        translation {
            match sType.Name.ToLowerInvariant() with
            | "boolean" ->
                let! translatedType = Type.translate sType

                let stringValue =
                    if value.ToString().ToLowerInvariant() = "false" then
                        "0"
                    else
                        "1"

                return translatedType, stringValue

            | typeName when typeName.EndsWith "[]" ->
                let array =
                    match typeName with
                    | "int32[]" -> value :?> array<int> |> Array.map string
                    | "byte[]" -> value :?> array<byte> |> Array.map string
                    | "single[]" -> value :?> array<float32> |> Array.map string
                    | _ -> raise <| InvalidKernelException $"Unsupported array type: %s{typeName}"

                let! translatedType =
                    Type.translate sType
                    |> State.using (fun ctx -> { ctx with ArrayKind = CArrayDecl array.Length })

                let stringValue = array |> String.concat ", " |> (fun s -> "{ " + s + "}")

                return translatedType, stringValue

            | _ ->
                let! translatedType = Type.translate sType
                // string null = ""
                let stringValue = string value
                return translatedType, stringValue
        }
        |> State.map (fun (type', value) -> Const(type', value))

    let translateVarSet (var: Var) (expr: Expr) =
        translation {
            let! var = translateVar var
            let! expr = translateCond (*TranslateAsExpr*) expr
            return Assignment(Property(PropertyType.Var var), expr)
        }

    let translateCond (cond: Expr) =
        translation {
            match cond with
            | Patterns.IfThenElse(if', then', else') ->
                let! l = translateCond if'
                let! r = translateCond then'
                let! e = translateCond else'
                let! isBoolAsBit = State.gets (fun context -> context.TranslatorOptions.BoolAsBit)

                let o1 =
                    match r with
                    | :? Const<Lang> as c when c.Val = "1" -> l
                    | _ -> Binop((if isBoolAsBit then BitAnd else And), l, r) :> Expression<_>

                match e with
                | :? Const<Lang> as c when c.Val = "0" -> return o1
                | _ -> return Binop((if isBoolAsBit then BitOr else Or), o1, e) :> Expression<_>

            | _ -> return! translateAsExpr cond
        }

    let translateIf (cond: Expr) (thenBranch: Expr) (elseBranch: Expr) =
        translation {
            let! if' = translateCond cond
            let! then' = translate thenBranch >>= toStb |> State.using clearContext

            let! else' =
                translation {
                    match elseBranch with
                    | Patterns.Value(null, sType) -> return None
                    | _ -> return! translate elseBranch >>= toStb |> State.using clearContext |> State.map Some
                }

            return IfThenElse(if', then', else')
        }

    // NOTE reversed loops not supported
    let translateForLoop (loopVar: Var) (from': Expr) (to': Expr) (step: Expr option) (body: Expr) =
        translation {
            let! loopVarName = State.gets (fun context -> context.Namer.LetStart loopVar.Name)
            let loopVarType = loopVar.Type

            let! loopVarBinding = translateBinding loopVar loopVarName from'

            let! condExpr = translateAsExpr to'
            let loopCond = Binop(LessEQ, Variable loopVarName, condExpr)

            do!
                State.modify (fun context ->
                    context.Namer.LetIn loopVar.Name
                    context)

            let! loopVarModifier =
                match step with
                | Some step ->
                    Expr.VarSet(
                        loopVar,
                        Expr.Call(
                            Utils.makeGenericMethodCall
                                [ loopVarType
                                  loopVarType
                                  loopVarType ]
                                <@ (+) @>,
                            [ Expr.Var loopVar
                              step ]
                        )
                    )
                    |> translate
                    |> State.map (fun node -> node :?> Statement<_>)
                | None -> translation { return Unop(UOp.Incr, Variable loopVarName) :> Statement<_> }

            let! loopBody = translate body >>= toStb |> State.using clearContext

            do!
                State.modify (fun context ->
                    context.Namer.LetOut()
                    context)

            return ForIntegerLoop(loopVarBinding, loopCond, loopVarModifier, loopBody)
        }

    let translateWhileLoop condExpr bodyExpr =
        translation {
            let! nCond = translateCond condExpr
            let! nBody = translate bodyExpr >>= toStb
            return WhileLoop(nCond, nBody)
        }

    let translateSeq expr1 expr2 =
        translation {
            let linearized = ResizeArray()

            let rec go expr =
                match expr with
                | Patterns.Sequential(e1, e2) ->
                    go e1
                    go e2
                | _ -> linearized.Add expr

            go expr1
            go expr2

            let! decls = State.gets (fun context -> ResizeArray(context.VarDecls))

            do!
                State.modify (fun context ->
                    context.VarDecls.Clear()
                    context)

            for expr in linearized do
                do!
                    State.modify (fun context ->
                        context.VarDecls.Clear()
                        context)

                match! translate expr with
                | :? StatementBlock<Lang> as s1 -> decls.AddRange(s1.Statements)
                | s1 -> decls.Add(s1 :?> Statement<_>)

            return StatementBlock decls
        }

    // TODO change to lambdas and applications without rec
    let translateApplication expr1 expr2 =
        translation {
            let rec go expr vals args =
                match expr with
                | Patterns.Lambda(v, e) -> go e vals (v :: args)
                | Patterns.Application(e1, e2) -> go e1 (e2 :: vals) args
                | _ ->
                    if vals.Length = args.Length then
                        let argsDict = vals |> List.zip (List.rev args) |> dict

                        //failwith "Partial evaluation is not supported in kernel function."
                        expr.Substitute(fun v -> if argsDict.ContainsKey v then Some argsDict.[v] else None), true
                    else
                        expr, false


            let (body, doing) = go expr1 [ expr2 ] []
            return body, doing
        }

    // TODO change to applications without rec
    let translateApplicationFun expr1 expr2 =
        translation {
            let rec go expr vals =
                translation {
                    match expr with
                    | Patterns.Application(e1, e2) ->
                        let! exp = translateAsExpr e2
                        return! go e1 (exp :: vals)
                    | _ ->
                        // TODO fix it: return exception rather than expr.ToString()
                        // NOTE не поддерживается частичное применение
                        // NOTE не поддерживается композиция функций (или функции высшего порядка)
                        let funName =
                            match expr with
                            | Patterns.ValueWithName(_, _, name) -> name
                            | _ -> expr.ToString()

                        return FunCall(funName, vals) :> Statement<_>
                }

            let! exp = translateAsExpr expr2
            return! go expr1 [ exp ]
        }

    let translateFieldSet host name value =
        translation {
            let! hostE = translateAsExpr host
            let! valE = translateAsExpr value
            return FieldSet(hostE, name, valE)
        }

    let translateStructFieldGet host name =
        translation {
            let! hostE = translateAsExpr host
            return FieldGet(hostE, name) :> Expression<_>
        }

    let translateUnionFieldGet expr (propInfo: PropertyInfo) =
        translation {
            let! unionType = State.gets (fun context -> context.CStructDecls.[expr.Type])
            let unionType = unionType :?> DiscriminatedUnionType<Lang>

            let! unionValueExpr = translateAsExpr expr

            // для структур du классы наследники кейсов кстати тоже не создаются :)
            let caseName = propInfo.DeclaringType.Name

            let unionCaseField =
                // для option классы наследники не создаются, поэтому нужно обрабатывать отдельно
                if caseName <> "FSharpOption`1" then
                    unionType.GetCaseByName caseName
                else
                    unionType.GetCaseByName "Some"

            match unionCaseField with
            | Some unionCaseField ->
                return
                    FieldGet(
                        FieldGet(FieldGet(unionValueExpr, unionType.Data.Name), unionCaseField.Name),
                        propInfo.Name
                    )
                    :> Expression<_>
            | None ->
                return
                    raise
                    <| InvalidKernelException
                        $"Union field get translation error: union %A{unionType.Name} doesn't have case %A{caseName}"
        }

    let private translateLet (var: Var) expr inExpr =
        translation {
            let! bName = State.gets (fun context -> context.Namer.LetStart var.Name)

            let! vDecl =
                translation {
                    match expr with
                    | DerivedPatterns.SpecificCall <@@ local @@> (_, _, _) ->
                        let! vType = Type.translate var.Type
                        return VarDecl(vType, bName, None, spaceModifier = Local)
                    | DerivedPatterns.SpecificCall <@@ localArray @@> (_, _, [ arg ]) ->
                        let! expr = translateCond arg

                        let arrayLength =
                            match expr with
                            | :? Const<Lang> as c -> int c.Val
                            | other ->
                                raise
                                <| InvalidKernelException $"Calling localArray with a non-const argument %A{other}"

                        let! arrayType =
                            Type.translate var.Type
                            |> State.using (fun ctx -> { ctx with ArrayKind = CArrayDecl arrayLength })

                        return VarDecl(arrayType, bName, None, spaceModifier = Local)
                    | Patterns.DefaultValue _ ->
                        let! vType = Type.translate var.Type
                        return VarDecl(vType, bName, None)
                    | _ -> return! translateBinding var bName expr
                }

            do!
                State.modify (fun context ->
                    context.VarDecls.Add vDecl
                    context)

            do!
                State.modify (fun context ->
                    context.Namer.LetIn var.Name
                    context)

            let! res = translate inExpr |> State.using clearContext
            let! sb = State.gets (fun context -> context.VarDecls)

            match res with
            | :? StatementBlock<Lang> as s -> sb.AddRange s.Statements
            | _ -> sb.Add(res :?> Statement<_>)

            do!
                State.modify (fun context ->
                    context.Namer.LetOut()
                    context)

            do! State.modify clearContext

            return StatementBlock sb :> Node<_>
        }

    let private translateProvidedCall expr =
        translation {
            let rec traverse expr args =
                translation {
                    match expr with
                    | Patterns.Value(calledName, sType) ->
                        match sType.Name.ToLowerInvariant() with
                        | "string" -> return (calledName :?> string), args
                        | _ ->
                            return
                                raise
                                <| TranslationFailedException
                                    $"Failed to parse provided call, expected string call name: {expr}"
                    | Patterns.Sequential(expr1, expr2) ->
                        let! updatedArgs =
                            translation {
                                match expr2 with
                                | Patterns.Value(null, _) -> return args // the last item in the sequence is null
                                | _ ->
                                    let! a = translateAsExpr expr2
                                    return a :: args
                            }

                        return! traverse expr1 updatedArgs
                    | _ -> return raise <| TranslationFailedException $"Failed to parse provided call: {expr}"
                }

            let! m = traverse expr []
            return FunCall m :> Node<_>
        }

    let translate expr =
        translation {
            let toNode (x: #Node<_>) = translation { return x :> Node<_> }

            match expr with
            | Patterns.AddressOf expr -> return raise <| InvalidKernelException $"AddressOf is not supported: {expr}"
            | Patterns.AddressSet expr -> return raise <| InvalidKernelException $"AddressSet is not supported: {expr}"

            | Patterns.Application(expr1, expr2) ->
                let! (e, applying) = translateApplication expr1 expr2

                if applying then
                    return! translate e
                else
                    return! translateApplicationFun expr1 expr2 >>= toNode

            | DerivedPatterns.SpecificCall <@@ print @@> (_, _, args) ->
                match args with
                | [ Patterns.ValueWithName(argTypes, _, _)
                    Patterns.ValueWithName(formatStr, _, _)
                    Patterns.ValueWithName(argValues, _, _) ] ->

                    let formatStrArg =
                        Const(PrimitiveType ConstStringLiteral, formatStr :?> string) :> Expression<_>

                    let! args' = translateListOfArgs (argValues :?> list<Expr>)
                    return FunCall("printf", formatStrArg :: args') :> Node<_>
                | _ -> return raise <| TranslationFailedException("printf: something going wrong.")

            | DerivedPatterns.SpecificCall <@ (|>) @> (_,
                                                       _,
                                                       [ expr
                                                         Patterns.Lambda(_,
                                                                         DerivedPatterns.SpecificCall <@ ignore @> (_,
                                                                                                                    _,
                                                                                                                    _)) ]) ->
                return! translate expr

            | DerivedPatterns.SpecificCall <@ LanguagePrimitives.GenericOne<int> @> (_, [ onType ], _) ->
                let! type' = Type.translate onType

                let value =
                    Expr
                        .Call(
                            Utils.makeGenericMethodCall [ onType ] <@ LanguagePrimitives.GenericOne<int> @>,
                            List.empty
                        )
                        .EvaluateUntyped()
                        .ToString()

                return Const(type', value) :> Node<_>

            | Patterns.Call(exprOpt, mInfo, args) -> return! translateCall exprOpt mInfo args >>= toNode
            | Patterns.Coerce(expr, sType) -> return raise <| InvalidKernelException $"Coerce is not supported: {expr}"
            | Patterns.DefaultValue sType ->
                return raise <| InvalidKernelException $"DefaultValue is not supported: {expr}"

            | Patterns.FieldGet(exprOpt, fldInfo) ->
                match exprOpt with
                | Some expr -> return! translateStructFieldGet expr fldInfo.Name >>= toNode
                | None ->
                    return
                        raise
                        <| InvalidKernelException $"FieldGet for empty host is not supported. Field: %A{fldInfo.Name}"

            | Patterns.FieldSet(exprOpt, fldInfo, expr) ->
                match exprOpt with
                | Some e -> return! translateFieldSet e fldInfo.Name expr >>= toNode
                | None ->
                    return
                        raise
                        <| InvalidKernelException $"Field set with empty host is not supported. Field: %A{fldInfo}"

            | ForLoopWithStep(loopVar, (start, step, finish), loopBody) ->
                return! translateForLoop loopVar start finish (Some step) loopBody >>= toNode
            | ForLoop(loopVar, (start, finish), loopBody) ->
                return! translateForLoop loopVar start finish None loopBody >>= toNode
            | Patterns.ForIntegerRangeLoop(loopVar, start, finish, loopBody) ->
                return! translateForLoop loopVar start finish None loopBody >>= toNode
            | Patterns.IfThenElse(cond, thenExpr, elseExpr) -> return! translateIf cond thenExpr elseExpr >>= toNode

            | Patterns.Lambda(var, _expr) -> return raise <| InvalidKernelException $"Lambda is not supported: %A{expr}"
            | Patterns.Let(var, expr, inExpr) ->
                match var.Name with
                | "___providedCallInfo" -> return! translateProvidedCall expr
                | _ -> return! translateLet var expr inExpr

            | Patterns.LetRecursive(bindings, expr) ->
                return raise <| InvalidKernelException $"LetRecursive is not supported: {expr}"
            | Patterns.NewArray(sType, exprs) ->
                return raise <| InvalidKernelException $"NewArray is not supported: {expr}"
            | Patterns.NewDelegate(sType, vars, expr) ->
                return raise <| InvalidKernelException $"NewDelegate is not supported: {expr}"

            | Patterns.NewObject(constrInfo, exprs) ->
                let! context = State.get
                // let p = constrInfo. GetParameters()
                // let p2 = constrInfo.GetMethodBody()
                let! structInfo = Type.translate constrInfo.DeclaringType
                let cArgs = exprs |> List.map (fun x -> translation { return! translateAsExpr x })

                return NewStruct<_>(structInfo :?> StructType<Lang>, cArgs |> List.map (State.eval context)) :> Node<_>

            | Patterns.NewRecord(sType, exprs) ->
                let! context = State.get
                let! structInfo = Type.translate sType
                let cArgs = exprs |> List.map (fun x -> translation { return! translateAsExpr x })

                return NewStruct<_>(structInfo :?> StructType<Lang>, cArgs |> List.map (State.eval context)) :> Node<_>

            | Patterns.NewTuple(exprs) ->
                let! context = State.get
                let! tupleDecl = Type.translate expr.Type
                let cArgs = exprs |> List.map (fun x -> translateAsExpr x)

                return NewStruct<_>(tupleDecl :?> StructType<Lang>, cArgs |> List.map (State.eval context)) :> Node<_>

            | Patterns.NewUnionCase(unionCaseInfo, exprs) ->
                let! context = State.get
                let! unionInfo = Type.translate unionCaseInfo.DeclaringType
                let unionInfo = unionInfo :?> DiscriminatedUnionType<Lang>

                let tag = Const(unionInfo.Tag.Type, string unionCaseInfo.Tag) :> Expression<_>

                let args =
                    match unionInfo.GetCaseByTag unionCaseInfo.Tag with
                    | None -> []
                    | Some field ->
                        let structArgs =
                            exprs |> List.map (fun x -> translateAsExpr x) |> List.map (State.eval context)

                        NewUnion(
                            unionInfo.Data.Type :?> UnionClInplaceType<_>,
                            field.Name,
                            NewStruct(field.Type :?> StructType<_>, structArgs)
                        )
                        :> Expression<_>
                        |> List.singleton

                return NewStruct(unionInfo, tag :: args) :> Node<_>

            | Patterns.PropertyGet(exprOpt, propInfo, exprs) ->
                return! translatePropGet exprOpt propInfo exprs >>= toNode
            | Patterns.PropertySet(exprOpt, propInfo, exprs, expr) ->
                return! translatePropSet exprOpt propInfo exprs expr >>= toNode
            | Patterns.Sequential(expr1, expr2) -> return! translateSeq expr1 expr2 >>= toNode
            | Patterns.TryFinally(tryExpr, finallyExpr) ->
                return raise <| InvalidKernelException $"TryFinally is not supported: {expr}"
            | Patterns.TryWith(expr1, var1, expr2, var2, expr3) ->
                return raise <| InvalidKernelException $"TryWith is not supported: {expr}"
            | Patterns.TupleGet(expr, i) -> return! translateStructFieldGet expr $"_{i + 1}" >>= toNode
            | Patterns.TypeTest(expr, sType) ->
                return raise <| InvalidKernelException $"TypeTest is not supported: {expr}"

            | Patterns.UnionCaseTest(expr, unionCaseInfo) ->
                let! unionInfo = Type.translate unionCaseInfo.DeclaringType
                let unionInfo = unionInfo :?> DiscriminatedUnionType<Lang>

                let! unionVarExpr = translateAsExpr expr
                let unionGetTagExpr = FieldGet(unionVarExpr, unionInfo.Tag.Name) :> Expression<_>
                // NOTE Const pog for genericOne
                let tagExpr = Const(unionInfo.Tag.Type, string unionCaseInfo.Tag) :> Expression<_>

                return Binop(EQ, unionGetTagExpr, tagExpr) :> Node<_>

            | Patterns.ValueWithName(obj', sType, name) ->
                let! context = State.get
                // Here is the only use of TranslationContext.InLocal
                if sType.ToString().EndsWith "[]" (*&& not context.InLocal*) then
                    context.Namer.AddVar name
                    let! res = translateValue obj' sType

                    context.TopLevelVarsDecls.Add(
                        VarDecl(res.Type, name, Some(res :> Expression<_>), AddressSpaceQualifier.Constant)
                    )

                    let var = Var(name, sType)
                    return! translateVar var >>= toNode
                else
                    return! translateValue obj' sType >>= toNode

            | Patterns.Value(obj', sType) -> return! translateValue obj' sType >>= toNode
            | Patterns.Var var -> return! translateVar var >>= toNode
            | Patterns.VarSet(var, expr) -> return! translateVarSet var expr >>= toNode
            | Patterns.WhileLoop(condExpr, bodyExpr) -> return! translateWhileLoop condExpr bodyExpr >>= toNode
            | _ ->
                return
                    raise
                    <| InvalidKernelException $"Following expression inside kernel is not supported:\n{expr}"
        }
