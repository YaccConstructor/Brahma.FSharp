﻿// Copyright (c) 2012, 2013 Semyon Grigorev <rsdpisuy@gmail.com>
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

module Brahma.FSharp.OpenCL.Translator.Body

open System.Reflection
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.AST
open Brahma.FSharp.OpenCL.QuotationsTransformer.Transformers.PrintfReplacer
open Microsoft.FSharp.Collections
open FSharpx.Collections
open System.Collections.Generic

let private clearContext (targetContext:TargetContext<'a,'b>) =
    let c = new TargetContext<'a,'b>()
    c.Namer <- targetContext.Namer
    c.Flags <- targetContext.Flags
    c.TopLevelVarsDeclarations <- targetContext.TopLevelVarsDeclarations
    for td in targetContext.tupleDecls do c.tupleDecls.Add(td.Key, td.Value)
    for t in targetContext.tupleList do c.tupleList.Add(t)
    c.tupleNumber <- targetContext.tupleNumber
    c.UserDefinedTypes.AddRange(targetContext.UserDefinedTypes)
    for kvp in targetContext.UserDefinedStructsOpenCLDeclaration do c.UserDefinedStructsOpenCLDeclaration.Add (kvp.Key,kvp.Value)
    for kvp in targetContext.UserDefinedUnionsOpenCLDeclaration do c.UserDefinedUnionsOpenCLDeclaration.Add (kvp.Key,kvp.Value)
    c

let rec private translateBinding (var:Var) newName (expr:Expr) (targetContext:TargetContext<_,_>) =
    let body,tContext = (*TranslateAsExpr*) translateCond expr targetContext
    let vType =
        match (body:Expression<_>) with
        | :? Const<Lang> as c -> c.Type
        | :? ArrayInitializer<Lang> as ai -> Type.Translate var.Type false (Some ai.Length) targetContext
        | _ -> Type.Translate var.Type false None targetContext
    new VarDecl<Lang>(vType,newName,Some body)

and private translateListOfArgs (args: list<Expr>) targetContext =
    let args', targetContext' =
        let a,c = args |> List.fold (fun (res,tc) a -> let r,tc = translateCond a tc in r::res,tc) ([],targetContext)
        a |> List.rev , c
    args', targetContext'

and private translateCall exprOpt (mInfo:System.Reflection.MethodInfo) _args targetContext =
    let args,tContext = translateListOfArgs _args targetContext

    match mInfo.Name.ToLowerInvariant() with
    | "op_multiply"            -> new Binop<_>(Mult,args.[0],args.[1]) :> Statement<_>,tContext
    | "op_addition"            -> new Binop<_>(Plus,args.[0],args.[1]) :> Statement<_>,tContext
    | "op_division"            -> new Binop<_>(Div,args.[0],args.[1]) :> Statement<_>,tContext
    | "op_lessthan"            -> new Binop<_>(Less,args.[0],args.[1]) :> Statement<_>,tContext
    | "op_lessthanorequal"     -> new Binop<_>(LessEQ,args.[0],args.[1]) :> Statement<_>,tContext
    | "op_greaterthan"         -> new Binop<_>(Great,args.[0],args.[1]) :> Statement<_>,tContext
    | "op_greaterthanorequal"  -> new Binop<_>(GreatEQ,args.[0],args.[1]) :> Statement<_>,tContext
    | "op_equality"            -> new Binop<_>(EQ,args.[0],args.[1]) :> Statement<_>,tContext
    | "op_inequality"          -> new Binop<_>(NEQ,args.[0],args.[1]) :> Statement<_>,tContext
    | "op_subtraction"         -> new Binop<_>(Minus,args.[0],args.[1]) :> Statement<_>,tContext
    | "op_unarynegation"       -> new Unop<_>(UOp.Minus,args.[0]) :> Statement<_>,tContext
    | "op_modulus"             -> new Binop<_>(Remainder,args.[0],args.[1]) :> Statement<_>,tContext
    | "op_bitwiseand"          -> new Binop<_>(BitAnd,args.[0],args.[1]) :> Statement<_>,tContext
    | "op_bitwiseor"           -> new Binop<_>(BitOr,args.[0],args.[1]) :> Statement<_>,tContext
    | "op_leftshift"           -> new Binop<_>(LeftShift,args.[0],args.[1]) :> Statement<_>,tContext
    | "op_rightshift"          -> new Binop<_>(RightShift,args.[0],args.[1]) :> Statement<_>,tContext
    | "op_booleanand"          -> new Binop<_>(And, args.[0], args.[1]) :> Statement<_>, tContext
    | "op_booleanor"           -> new Binop<_>(Or, args.[0], args.[1]) :> Statement<_>, tContext
    | "op_lessbangplusgreater"
    | "op_lessbangplus"        ->
        tContext.Flags.enableAtomic <- true
        new FunCall<_>("atom_add",[new Pointer<_>(args.[0]);args.[1]]) :> Statement<_>,tContext
    | "op_lessbangmunus"
    | "op_lessbangmunusgreater"->
        tContext.Flags.enableAtomic <- true
        new FunCall<_>("atom_sub",[new Pointer<_>(args.[0]);args.[1]]) :> Statement<_>,tContext
    | "op_lessbanggreater"
    | "op_lessbang"           ->
        tContext.Flags.enableAtomic <- true
        new FunCall<_>("atom_xchg",[new Pointer<_>(args.[0]);args.[1]]) :> Statement<_>,tContext
    | "amax" | "amaxr"        ->
        tContext.Flags.enableAtomic <- true
        new FunCall<_>("atom_max",[new Pointer<_>(args.[0]);args.[1]]) :> Statement<_>,tContext
    | "amin" | "aminr"        ->
        tContext.Flags.enableAtomic <- true
        new FunCall<_>("atom_min",[new Pointer<_>(args.[0]);args.[1]]) :> Statement<_>,tContext
    | "aincr" | "aincrr"      ->
        tContext.Flags.enableAtomic <- true
        new FunCall<_>("atom_inc",[new Pointer<_>(args.[0])]) :> Statement<_>,tContext
    | "adecr" | "adecrr"      ->
        tContext.Flags.enableAtomic <- true
        new FunCall<_>("atom_dec",[new Pointer<_>(args.[0])]) :> Statement<_>,tContext
    | "acompexch" | "acompexchr"      ->
        tContext.Flags.enableAtomic <- true
        new FunCall<_>("atom_cmpxchg",[new Pointer<_>(args.[0]);args.[1];args.[2]]) :> Statement<_>,tContext

    | "todouble"               -> new Cast<_>( args.[0],new PrimitiveType<_>(PTypes<_>.Float)):> Statement<_>,tContext
    | "toint"                  -> new Cast<_>( args.[0],new PrimitiveType<_>(PTypes<_>.Int)):> Statement<_>,tContext
    | "toint16"                -> new Cast<_>( args.[0],new PrimitiveType<_>(PTypes<_>.Short)):> Statement<_>,tContext
    | "tosingle"               -> new Cast<_>( args.[0],new PrimitiveType<_>(PTypes<_>.Float)):> Statement<_>,tContext
    | "tobyte"                 -> new Cast<_>( args.[0],new PrimitiveType<_>(PTypes<_>.UChar)):> Statement<_>,tContext
    | "touint32"               -> new Cast<_>( args.[0],new PrimitiveType<_>(PTypes<_>.UInt)):> Statement<_>,tContext
    | "touint16"               -> new Cast<_>( args.[0],new PrimitiveType<_>(PTypes<_>.UShort)):> Statement<_>,tContext
    | "toint64"                -> new Cast<_>( args.[0],new PrimitiveType<_>(PTypes<_>.Long)):> Statement<_>,tContext
    | "touint64"               -> new Cast<_>( args.[0],new PrimitiveType<_>(PTypes<_>.ULong)):> Statement<_>,tContext
    | "acos" | "asin" | "atan"
    | "cos" | "cosh" | "exp"
    | "floor" | "log" | "log10"
    | "pow" | "sin" | "sinh" | "sqrt"
    | "tan" | "tanh" as fName ->
        if mInfo.DeclaringType.AssemblyQualifiedName.StartsWith("System.Math")
            || mInfo.DeclaringType.AssemblyQualifiedName.StartsWith("Microsoft.FSharp.Core.Operators")
        then FunCall<_>(fName,args) :> Statement<_>,tContext
        else failwithf "Seems, thet you use math function with name %s not from System.Math. or Microsoft.FSharp.Core.Operators" fName
    | "abs" as fName ->
        if mInfo.DeclaringType.AssemblyQualifiedName.StartsWith("Microsoft.FSharp.Core.Operators")
        then FunCall<_>("fabs",args) :> Statement<_>,tContext
        else failwithf "Seems, thet you use math function with name %s not from System.Math. or Microsoft.FSharp.Core.Operators" fName
    | "powinteger" as fName ->
        if mInfo.DeclaringType.AssemblyQualifiedName.StartsWith("Microsoft.FSharp.Core.Operators")
        then FunCall<_>("powr",args) :> Statement<_>,tContext
        else failwithf "Seems, thet you use math function with name %s not from System.Math. or Microsoft.FSharp.Core.Operators" fName
    | "ref" ->
        Pointer<_> args.[0] :> Statement<_>, tContext
    | "op_dereference" ->
        IndirectionOp<_> args.[0] :> Statement<_>, tContext
    | "op_colonequals" ->
        Assignment<_>(Property<_>(PropertyType.VarReference(IndirectionOp<_> args.[0])), args.[1]) :> Statement<_>, tContext
    | "setarray" ->
        let item = new Item<_>(args.[0],args.[1])
        new Assignment<_>(new Property<_>(PropertyType.Item(item)),args.[2]) :> Statement<_>
        , tContext
    | "getarray" -> new Item<_>(args.[0],args.[1]) :> Statement<_>, tContext
    | "not" ->  new Unop<_>(UOp.Not,args.[0]) :> Statement<_>,tContext
    | "_byte"    -> args.[0] :> Statement<_>, tContext
    | "barrier" -> new Barrier<_>() :> Statement<_>, tContext
    | "local"    -> failwith "Calling the local function is allowed only at the top level of the let binding"
    | "arrayLocal" -> failwith "Calling the localArray function is allowed only at the top level of the let binding"
    | "zerocreate" ->
        let length =
            match args.[0] with
            | :? Const<Lang> as c -> int c.Val
            | other -> failwithf "Calling Array.zeroCreate with a non-const argument: %A" other
        new ZeroArray<_>(length) :> Statement<_>, tContext
    | "fst" -> new FieldGet<_>(args.[0], "_1") :> Statement<_>, tContext
    | "snd" -> new FieldGet<_>(args.[0], "_2") :> Statement<_>, tContext
    | "first" -> new FieldGet<_>(args.[0], "_1") :> Statement<_>, tContext
    | "second" -> new FieldGet<_>(args.[0], "_2") :> Statement<_>, tContext
    | "third" -> new FieldGet<_>(args.[0], "_3") :> Statement<_>, tContext
    | c -> failwithf "Unsupported call: %s" c

and private itemHelper exprs hostVar tContext =
    let idx,tContext =
        match exprs with
        | hd::_ -> TranslateAsExpr hd tContext
        | [] -> failwith "Array index missed!"
    let (hVar, _) = hostVar

    idx, tContext, hVar

and private translateSpecificPropGet expr propName exprs targetContext =
    // TODO: Refactoring: Safe pattern matching by expr type.

    let hostVar = TranslateAsExpr expr targetContext
    match propName with
    | "globalid0i" | "globalid0" -> FunCall<_>("get_global_id",[Const(PrimitiveType<_>(Int),"0")]) :> Expression<_>, targetContext
    | "globalid1i" | "globalid1" ->  FunCall<_>("get_global_id",[Const(PrimitiveType<_>(Int),"1")]) :> Expression<_>, targetContext
    | "globalid2i" | "globalid2" ->  FunCall<_>("get_global_id",[Const(PrimitiveType<_>(Int),"2")]) :> Expression<_>, targetContext
    | "localid0" -> FunCall<_>("get_local_id",[Const(PrimitiveType<_>(Int),"0")]) :> Expression<_>, targetContext
    | "localid1" -> FunCall<_>("get_local_id",[Const(PrimitiveType<_>(Int),"1")]) :> Expression<_>, targetContext
    | "localid2" -> FunCall<_>("get_local_id",[Const(PrimitiveType<_>(Int),"2")]) :> Expression<_>, targetContext
    | "item" ->
        let idx,tContext,hVar = itemHelper exprs hostVar targetContext
        Item<_>(hVar,idx) :> Expression<_>, tContext
    | _ -> failwithf "Unsupported property in kernel: %A" propName

and private translatePropGet (exprOpt: Expr Option) (propInfo: PropertyInfo) exprs (targetContext: TargetContext<_, _>) =
    let propName = propInfo.Name.ToLowerInvariant()

    match exprOpt with
    | Some expr ->
        let exprType = expr.Type
        if targetContext.UserDefinedTypes.Contains exprType
        then
            let exprTypeName = expr.Type.Name.ToLowerInvariant()
            if targetContext.UserDefinedStructsOpenCLDeclaration.ContainsKey exprTypeName
            then
                translateStructFieldGet expr propInfo.Name targetContext
            else
                translateUnionFieldGet expr propInfo targetContext
        else
            translateSpecificPropGet expr propName exprs targetContext
    | None -> failwithf "Unsupported static property get in kernel: %A" propName

and private translatePropSet exprOpt (propInfo:System.Reflection.PropertyInfo) exprs newVal targetContext =
    // Todo: Safe pattern matching (item) by expr type
    let propName = propInfo.Name.ToLowerInvariant()

    match exprOpt with
    | Some expr ->
        let hostVar = TranslateAsExpr expr targetContext
        let newVal,tContext = TranslateAsExpr newVal (match hostVar with (v,c) -> c)
        match propInfo.Name.ToLowerInvariant() with
        | "item" ->
            let idx,tContext,hVar = itemHelper exprs hostVar tContext
            let item = new Item<_>(hVar,idx)
            new Assignment<_>(new Property<_>(PropertyType.Item(item)),newVal) :> Statement<_>
            , tContext
        | _ ->
            let r,tContext = translateFieldSet expr propInfo.Name exprs.[0] targetContext
            r :> Statement<_>,tContext
    | None -> failwithf "Unsupported static property set in kernel: %A" propName

and TranslateAsExpr expr (targetContext:TargetContext<_,_>) =
    let (r:Node<_>),tc = Translate expr (targetContext:TargetContext<_,_>)
    (r  :?> Expression<_>) ,tc

and getVar (clVarName:string) (targetContext:TargetContext<_,_>) =
    new Variable<_>(clVarName)

and translateVar (var:Var) (targetContext:TargetContext<_,_>) =
    //getVar var.Name targetContext
    let vName = targetContext.Namer.GetCLVarName var.Name
    match vName with
    | Some n -> getVar n targetContext
    | None ->
            sprintf "Seems, that you try to use variable with name %A, that declared out of quotation." var.Name
          + "Please, pass it as quoted function's parametaer."
          |> failwith

and translateValue (value:obj) (sType:System.Type) targetContext =
    let mutable _type = None
    let v =
        let s = string value
        match sType.Name.ToLowerInvariant() with
        | "boolean" ->
            _type <- Type.Translate sType false None targetContext |> Some
            if s.ToLowerInvariant() = "false" then "0" else "1"
        | t when t.EndsWith "[]" ->
            let arr =
                match t with
                | "int32[]" -> value :?> array<int> |> Array.map string
                | "byte[]" -> value :?> array<byte> |> Array.map string
                | "single[]" -> value :?> array<float32> |> Array.map string
                | _ -> failwith "Unsupported array type."
            _type <- Type.Translate sType false (Some arr.Length) targetContext |> Some
            arr |> String.concat ", "
            |> fun s -> "{ " + s + "}"
        | _ ->
            _type <- Type.Translate sType false None targetContext |> Some
            s
    new Const<_>(_type.Value, v)

and translateVarSet (var:Var) (expr:Expr) targetContext =
    let var = translateVar var targetContext
    let expr,tContext = translateCond(*TranslateAsExpr*) expr targetContext
    new Assignment<_>(new Property<_>(PropertyType.Var var),expr),tContext

and translateCond (cond:Expr) targetContext =
    match cond with
    | Patterns.IfThenElse(cond,_then,_else) ->
        let l,tContext = translateCond cond targetContext
        let r,tContext = translateCond _then tContext
        let e,tContext = translateCond _else tContext
        let asBit = tContext.TranslatorOptions.Contains(BoolAsBit)
        let o1 =
            match r with
            | :? Const<Lang> as c when c.Val = "1" -> l
            | _ -> new Binop<_>((if asBit then BitAnd else And),l,r) :> Expression<_>
        match e with
        | :? Const<Lang> as c when c.Val = "0" -> o1
        | _ -> new Binop<_>((if asBit then BitOr else Or), o1, e) :> Expression<_>
        , tContext
    | _ -> TranslateAsExpr cond targetContext

and toStb (s:Node<_>) =
    match s with
    | :? StatementBlock<_> as s -> s
    | x -> new StatementBlock<_>(new ResizeArray<_>([x :?> Statement<_>]))

and translateIf (cond:Expr) (thenBranch:Expr) (elseBranch:Expr) targetContext =
    let cond,tContext = translateCond cond targetContext
    let _then,tContext =
        let t,tc = Translate thenBranch (clearContext targetContext)
        toStb t, tc
    let _else,tContext =
        match elseBranch with
        | Patterns.Value(null,sType) -> None,tContext
        | _ ->
            let r,tContext = Translate elseBranch (clearContext targetContext)
            Some (toStb r), tContext
    new IfThenElse<_>(cond,_then, _else), targetContext

and translateForIntegerRangeLoop (i:Var) (from:Expr) (_to:Expr) (_do:Expr) (targetContext:TargetContext<_,_>) =
    let iName = targetContext.Namer.LetStart i.Name
    let v = getVar iName targetContext
    let var = translateBinding i iName from targetContext
    let condExpr,tContext = TranslateAsExpr _to targetContext
    targetContext.Namer.LetIn i.Name
    let body,tContext = Translate _do (clearContext targetContext)
    let cond = new Binop<_>(LessEQ, v, condExpr)
    let condModifier = new Unop<_>(UOp.Incr,v)
    targetContext.Namer.LetOut()
    new ForIntegerLoop<_>(var,cond, condModifier,toStb body),targetContext

and translateWhileLoop condExpr bodyExpr targetContext =
    let nCond,tContext = translateCond condExpr targetContext
    let nBody,tContext = Translate bodyExpr tContext
    new WhileLoop<_>(nCond, toStb nBody), tContext

and translateSeq expr1 expr2 (targetContext:TargetContext<_,_>) =
    let linearized = new ResizeArray<_>()
    let rec go e =
        match e with
        | Patterns.Sequential(e1,e2) ->
            go e1
            go e2
        | e -> linearized.Add e
    go expr1
    go expr2
    let decls = new ResizeArray<_>(targetContext.VarDecls)
    targetContext.VarDecls.Clear()
    let tContext =
        linearized
        |> ResizeArray.fold
            (fun (context:TargetContext<_,_>) s ->
                context.VarDecls.Clear()
                let nExpr,tContext = Translate s targetContext
                match nExpr:Node<_> with
                | :? StatementBlock<Lang> as s1 -> decls.AddRange(s1.Statements)
                | s1 -> decls.Add(s1:?> Statement<_>)
                tContext
                )
            targetContext
    let stmt = new StatementBlock<Lang>(decls)
    stmt, tContext

and translateApplication expr1 expr2 targetContext =
    let rec go expr _vals args  =
        match expr with
        | Patterns.Lambda (v,e) ->
            go e _vals (v::args)
        | Patterns.Application (e1,e2) ->
            go e1 (e2::_vals) args
        | e ->
            if _vals.Length = args.Length then
                let d =
                    List.zip (List.rev args) _vals |> dict

                    //failwith "Partial evaluation is not supported in kernel function."
                e.Substitute (fun v -> if d.ContainsKey v then Some d.[v] else None) ,true
            else e, false
    let body, doing = go expr1 [expr2] []
    body, doing, targetContext
    //if(body = null) then
    //    translateApplicationFun expr1 expr2 targetContext
    //else

                //else
                //let getStatementFun = dictionaryFun.[expr.
                    //new FunCall<_>(expr.ToString(), _vals) :> Statement<_>,targetContext
                    //failwith "-Partial evaluation is not supported in kernel function."

and translateApplicationFun expr1 expr2 targetContext =
    let rec go expr _vals args =
        match expr with
        | Patterns.Lambda (v,e) ->
            go e _vals (v::args)
        | Patterns.Application (e1,e2) ->
            let exp, tc = (TranslateAsExpr (e2) targetContext)
            go e1 (exp::_vals) args
        | e ->
            let listArg = List.rev _vals
            let funName =
                match expr with
                | Patterns.ValueWithName(_, _, name) -> name
                | _ -> expr.ToString()
            let funCall = new FunCall<_>(funName, _vals) :> Statement<_>
            funCall, targetContext
                //failwith "-Partial evaluation is not supported in kernel function."
    let exp, tc = (TranslateAsExpr (expr2) targetContext)
    go expr1 [exp] []

and translateFieldSet host (*fldInfo:System.Reflection.FieldInfo*) name _val context =
    let hostE, tc = TranslateAsExpr host context
    let field = name//fldInfo.Name
    let valE, tc = TranslateAsExpr _val tc
    let res = new FieldSet<_>(hostE, field, valE)
    res, tc




and translateStructFieldGet host (*fldInfo:System.Reflection.FieldInfo*) name context =
    let hostE, tc = TranslateAsExpr host context
    let field = name //fldInfo.Name
    let res = FieldGet<_>(hostE,field) :> Expression<_>
    res, tc

and translateUnionFieldGet expr (propInfo: PropertyInfo) targetContext =
    let exprTypeName = expr.Type.Name.ToLowerInvariant()
    let unionType = targetContext.UserDefinedUnionsOpenCLDeclaration.[exprTypeName]

    let unionValueExpr, targetContext = TranslateAsExpr expr targetContext

    let caseName = propInfo.DeclaringType.Name
    let unionCaseField = unionType.GetCaseByName caseName

    match unionCaseField with
    | None -> failwithf "Union field get translation error:
                         union %A doesn't have case %A" unionType.Name caseName
    | Some unionCaseField ->
        let r =
            FieldGet<_> (
                FieldGet<_> (
                    FieldGet<_> (
                        unionValueExpr,
                        unionType.Data.Name
                    ),
                    unionCaseField.Name
                ),
                propInfo.Name
            )
            :> Expression<_>
        r, targetContext

and Translate expr (targetContext:TargetContext<_,_>) =
    match expr with
    | Patterns.AddressOf expr -> "AdressOf is not suported:" + string expr|> failwith
    | Patterns.AddressSet expr -> "AdressSet is not suported:" + string expr|> failwith
    | Patterns.Application (expr1,expr2) ->
        let e, appling, targetContext = translateApplication expr1 expr2 targetContext
        if(appling) then
            Translate e targetContext
        else
            let r, tContext= translateApplicationFun expr1 expr2 targetContext
            r :> Node<_>,tContext
    | DerivedPatterns.SpecificCall <@@ print @@>(_, _, args) ->
        match args with
        | [Patterns.ValueWithName(argTypes, _, _)
           Patterns.ValueWithName(formatStr, _, _)
           Patterns.ValueWithName(argValues, _, _)] ->
                let formatStrArg =
                    Const(PrimitiveType ConstStringLiteral,
                          formatStr :?> string)
                    :> Expression<_>
                let args', targetContext' = translateListOfArgs (argValues :?> list<Expr>) targetContext
                FunCall("printf", formatStrArg :: args') :> Node<_>, targetContext'
        | _ -> failwith "printf: something going wrong."
    | Patterns.Call (exprOpt,mInfo,args) ->
        let r,tContext = translateCall exprOpt mInfo args targetContext
        r :> Node<_>,tContext
    | Patterns.Coerce(expr,sType) -> "Coerce is not suported:" + string expr|> failwith
    | Patterns.DefaultValue sType -> "DefaulValue is not suported:" + string expr|> failwith
    | Patterns.FieldGet (exprOpt,fldInfo) ->
        match exprOpt with
        | Some expr ->
            let r,tContext = translateStructFieldGet expr fldInfo.Name targetContext
            r :> Node<_>,tContext
        | None -> failwithf "FieldGet for empty host is not suported. Field: %A" fldInfo.Name
    | Patterns.FieldSet (exprOpt,fldInfo,expr) ->
        match exprOpt with
        | Some e ->
            let r,tContext = translateFieldSet e fldInfo.Name expr targetContext
            r :> Node<_>,tContext
        | None -> failwithf "Fileld set with empty host is not supported. Field: %A" fldInfo
    | Patterns.ForIntegerRangeLoop (i, from, _to, _do) ->
        let r,tContext = translateForIntegerRangeLoop i from _to _do targetContext
        r :> Node<_>, tContext
    | Patterns.IfThenElse (cond, thenExpr, elseExpr) ->
        let r,tContext = translateIf cond thenExpr elseExpr targetContext
        r :> Node<_>, tContext
    | Patterns.Lambda (var,_expr) ->
       // translateLambda var expr targetContext
        "Lambda is not suported:" + string expr|> failwith
    | Patterns.Let (var, expr, inExpr) ->
        match var.Name with
        | "___providedCallInfo" -> translateProvidedCall expr targetContext
        | _ -> translateLet var expr inExpr targetContext

    | Patterns.LetRecursive (bindings,expr) -> "LetRecursive is not suported:" + string expr|> failwith
    | Patterns.NewArray(sType,exprs) -> "NewArray is not suported:" + string expr|> failwith
    | Patterns.NewDelegate(sType,vars,expr) -> "NewDelegate is not suported:" + string expr|> failwith
    | Patterns.NewObject(constrInfo, exprs) ->
        let p = constrInfo.GetParameters()
        let p2 = constrInfo.GetMethodBody()
        if targetContext.UserDefinedTypes.Contains(constrInfo.DeclaringType)
        then
            let structInfo =  targetContext.UserDefinedStructsOpenCLDeclaration.[constrInfo.DeclaringType.Name.ToLowerInvariant()]
            let cArgs = exprs |> List.map (fun x -> TranslateAsExpr x targetContext)
            let res = NewStruct<_>(structInfo, cArgs |> List.unzip |> fst)
            res :> Node<_>,targetContext
        else "NewObject is not suported:" + string expr|> failwith
    | Patterns.NewRecord(sType,exprs) -> "NewRecord is not suported:" + string expr|> failwith
    | Patterns.NewTuple(exprs) ->
        let mutable n = 0
        let baseTypes = [|for i in 0..exprs.Length - 1 -> exprs.[i].Type|]
        let elements = [for i in 0..exprs.Length - 1 -> { Name = "_" + (i + 1).ToString(); Type = Type.Translate baseTypes.[i] false None targetContext}]
        let mutable s = ""
        for i in 0..baseTypes.Length - 1 do s <- s + baseTypes.[i].Name
        if not (targetContext.tupleDecls.ContainsKey(s))
        then
            targetContext.tupleNumber <- targetContext.tupleNumber + 1
            targetContext.tupleDecls.Add(s, targetContext.tupleNumber)
            let a = StructType<Lang>("tuple" + targetContext.tupleNumber.ToString(), elements)
            targetContext.tupleList.Add(a)
            let cArgs = exprs |> List.map (fun x -> TranslateAsExpr x targetContext)
            NewStruct<_>(a,cArgs |> List.unzip |> fst) :> Node<_>, targetContext
        else
            let a = StructType<Lang>("tuple" + (targetContext.tupleDecls.Item(s)).ToString(), elements)
            let cArgs = exprs |> List.map (fun x -> TranslateAsExpr x targetContext)
            NewStruct<_>(a,cArgs |> List.unzip |> fst) :> Node<_>, targetContext
    | Patterns.NewUnionCase(unionCaseInfo, exprs) ->
        let unionType = unionCaseInfo.DeclaringType
        if not <| targetContext.UserDefinedTypes.Contains(unionType)
        then
            failwithf "Union type %s is not registered" unionType.Name

        let typeName = unionType.Name.ToLowerInvariant()
        let unionInfo = targetContext.UserDefinedUnionsOpenCLDeclaration.[typeName]


        let tag = Const(unionInfo.Tag.Type, string unionCaseInfo.Tag) :> Expression<_>
        let args =
            match unionInfo.GetCaseByTag unionCaseInfo.Tag with
            | None -> []
            | Some field ->
                let structArgs =
                    exprs |> List.map (fun x -> fst <| TranslateAsExpr x targetContext)
                let data =
                    NewUnion (
                        unionInfo.Data.Type :?> UnionClInplaceType<_>,
                        field.Name,
                        NewStruct (
                            field.Type :?> StructType<_>,
                            structArgs
                        )
                    )
                [data :> Expression<_>]

        NewStruct(unionInfo, tag :: args) :> Node<_>, targetContext
    | Patterns.PropertyGet(exprOpt, propInfo, exprs) ->
        let res, tContext = translatePropGet exprOpt propInfo exprs targetContext
        (res :> Node<_>), tContext
    | Patterns.PropertySet(exprOpt,propInfo,exprs,expr) ->
        let res,tContext = translatePropSet exprOpt propInfo exprs expr targetContext
        res :> Node<_>,tContext
    | Patterns.Sequential(expr1,expr2) ->
        let res,tContext = translateSeq expr1 expr2 targetContext
        res :> Node<_>,tContext
    | Patterns.TryFinally(tryExpr,finallyExpr) -> "TryFinally is not suported:" + string expr|> failwith
    | Patterns.TryWith(expr1,var1,expr2,var2,expr3) -> "TryWith is not suported:" + string expr|> failwith
    | Patterns.TupleGet(expr,i) ->
        let r,tContext =  translateStructFieldGet expr  ("_" + (string (i + 1))) targetContext
        r :> Node<_>,tContext
    | Patterns.TypeTest(expr, sType) -> "TypeTest is not suported:" + string expr|> failwith
    | Patterns.UnionCaseTest(expr, unionCaseInfo) ->
        let unionTypeName = expr.Type.Name.ToLowerInvariant()
        let unionDecl = targetContext.UserDefinedUnionsOpenCLDeclaration.[unionTypeName]

        let unionVarExpr, tc = TranslateAsExpr expr targetContext
        let unionGetTagExpr = FieldGet(unionVarExpr, unionDecl.Tag.Name) :> Expression<_>
        let tagExpr = Const(unionDecl.Tag.Type, string unionCaseInfo.Tag) :> Expression<_>

        Binop(BOp.EQ, unionGetTagExpr, tagExpr) :> Node<_>, tc
    | Patterns.ValueWithName(_obj,sType,name) ->
        // Here is the only use of TargetContext.InLocal
        if sType.ToString().EndsWith "[]" && not targetContext.InLocal
        then
            targetContext.Namer.AddVar name
            let res = translateValue _obj sType  targetContext
            targetContext.TopLevelVarsDeclarations.Add(new VarDecl<_>(res.Type, name, Some(res :> Expression<_>), AddressSpaceQualifier.Constant))
            let var = new Var(name, sType)
            translateVar var targetContext :> Node<_>, targetContext
        else translateValue _obj sType  targetContext :> Node<_> , targetContext
    | Patterns.Value(_obj,sType) -> translateValue _obj sType  targetContext :> Node<_> , targetContext
    | Patterns.Var var -> translateVar var targetContext :> Node<_>, targetContext
    | Patterns.VarSet(var,expr) ->
        let res,tContext = translateVarSet var expr targetContext
        res :> Node<_>,tContext
    | Patterns.WhileLoop(condExpr,bodyExpr) ->
        let r,tContext = translateWhileLoop condExpr bodyExpr targetContext
        r :> Node<_>, tContext
    | other -> "OTHER!!! :" + string other |> failwith


and private translateLet var expr inExpr (targetContext:TargetContext<_,_>) =
    let bName = targetContext.Namer.LetStart var.Name

    let vDecl =
        match expr with
        | DerivedPatterns.SpecificCall <@@ local @@> (_,_,_) ->
            let vType = Type.Translate var.Type false None targetContext
            VarDecl<Lang>(vType,bName,None,spaceModifier=Local)
        | DerivedPatterns.SpecificCall <@@ localArray @@> (_,_,[arg]) ->
            (*newTargetContext?*)
            let expr, newTargetContext = translateCond arg targetContext
            let length =
                match expr with
                | :? Const<Lang> as c -> (Some << int) c.Val
                | other -> sprintf "Calling localArray with a non-const argument %A" other |> failwith
            let arrayType =
                Type.Translate var.Type false length newTargetContext
            VarDecl<Lang>(arrayType,bName,None,spaceModifier=Local)
        | _ -> translateBinding var bName expr targetContext

    targetContext.VarDecls.Add vDecl
    targetContext.Namer.LetIn var.Name

    let res,tContext = clearContext targetContext |> Translate inExpr //вот тут мб нужно проверять на call или application
    let sb = new ResizeArray<_>(targetContext.VarDecls |> Seq.cast<Statement<_>>)
    targetContext.tupleDecls.Clear()
    targetContext.tupleList.Clear()
    for td in tContext.tupleDecls do targetContext.tupleDecls.Add(td.Key, td.Value)
    for t in tContext.tupleList do targetContext.tupleList.Add(t)
    targetContext.tupleNumber <- tContext.tupleNumber
    match res with
    | :? StatementBlock<Lang> as s -> sb.AddRange s.Statements;
    | _ -> sb.Add (res :?> Statement<_>)


    targetContext.Namer.LetOut()
    new StatementBlock<_>(sb) :> Node<_>
    , (clearContext targetContext)

and private translateProvidedCall expr (targetContext:TargetContext<_,_>) =
    let rec traverse expr args =
        match expr with
        | Patterns.Value(calledName, sType) ->
            match sType.Name.ToLowerInvariant() with
            | "string" -> (calledName :?> string), args
            | _ -> "Failed to parse provided call, expected string call name: " + string expr |> failwith
        | Patterns.Sequential(expr1, expr2) ->
            let updatedArgs =
                match expr2 with
                | Patterns.Value(null, _) -> args // the last item in the sequence is null
                | _ -> (TranslateAsExpr (expr2) targetContext |> fst)::args
            traverse expr1 updatedArgs
        | _ -> "Failed to parse provided call: " + string expr |> failwith
    let funCall = new FunCall<_>(traverse expr []) :> Node<_>
    funCall, targetContext
