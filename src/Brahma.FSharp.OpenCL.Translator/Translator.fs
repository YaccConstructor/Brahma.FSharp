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

open Microsoft.FSharp.Quotations
open  Brahma.FSharp.OpenCL.AST
open System.Collections.Generic
open Brahma.FSharp.OpenCL.Translator.Type

type FSQuotationToOpenCLTranslator() =
   
    let CollectStructs e =
        let escapeNames = [|"_1D";"_2D";"_3D"|]
        let structs = new System.Collections.Generic.Dictionary<System.Type, _> () 
        let  add (t:System.Type) =
            if ((t.IsValueType && not t.IsPrimitive && not t.IsEnum)) && not (structs.ContainsKey t) 
               && not (Array.exists ((=)t.Name) escapeNames )
            then structs.Add(t, ())
        let rec go (e: Expr) = 
            add e.Type
            match e with
            | ExprShape.ShapeVar(v) -> ()
            | ExprShape.ShapeLambda(v, body) -> go body            
            | ExprShape.ShapeCombination(o, l) ->
                o.GetType() |> add 
                List.iter go l
        go e
        structs

    /// The parameter 'vars' is an immutable map that assigns expressions to variables
    /// (as we recursively process the tree, we replace all known variables)
    let rec expand vars expr = 

      // First recursively process & replace variables
      let expanded = 
        match expr with
        // If the variable has an assignment, then replace it with the expression
        | ExprShape.ShapeVar v when Map.containsKey v vars -> vars.[v]
        // Apply 'expand' recursively on all sub-expressions
        | ExprShape.ShapeVar v -> Expr.Var v
        | Patterns.Call(body, DerivedPatterns.MethodWithReflectedDefinition meth, args) ->
            let this = match body with Some b -> Expr.Application(meth, b) | _ -> meth
            let res = Expr.Applications(this, [ for a in args -> [a]])
            expand vars res
        | ExprShape.ShapeLambda(v, expr) -> 
            Expr.Lambda(v, expand vars expr)
        | ExprShape.ShapeCombination(o, exprs) ->
            ExprShape.RebuildShapeCombination(o, List.map (expand vars) exprs)

      // After expanding, try reducing the expression - we can replace 'let'
      // expressions and applications where the first argument is lambda
      match expanded with
      | Patterns.Application(ExprShape.ShapeLambda(v, body), assign)
      | Patterns.Let(v, assign, body) ->
          expand (Map.add v (expand vars assign) vars) body
      | _ -> expanded

    let addReturn subAST = 
        let rec adding (stmt:Statement<'lang>) =
            match stmt with
            | :? StatementBlock<'lang> as sb -> 
                let listStaments = sb.Statements
                let lastStatement = listStaments.[listStaments.Count - 1]
                sb.Remove (listStaments.Count - 1)             
                sb.Append(adding lastStatement)
                sb :> Statement<_>
            | :? Expression<'lang> as ex -> (new Return<_>(ex)) :> Statement<_>
            | :? IfThenElse<'lang> as ite -> 
                let newThen = (adding (ite.Then)) :?> StatementBlock<_>
                let newElse =
                    if(ite.Else = None) then
                        None
                    else
                        Some((adding (ite.Else.Value)) :?> StatementBlock<_> )
                (new IfThenElse<_>(ite.Condition,newThen, newElse)) :> Statement<_>
            | _ -> failwithf "Unsapported statement to add Return: %A" stmt

        adding subAST


    let mutable newAST = new ResizeArray<Method>()
    let brahmaDimensionsTypes = ["_1d";"_2d";"_3d"]
    let brahmaDimensionsTypesPrefix = "brahma.opencl."
    let bdts = brahmaDimensionsTypes |> List.map (fun s -> brahmaDimensionsTypesPrefix + s)
    let buildFullAst (varsList:ResizeArray<_>) types (partialAstList:ResizeArray<_>) (contextList:ResizeArray<TargetContext<_,_>>) =
        let mutable listCLFun = []
        for i in 0..(varsList.Count-1) do
            let formalArgs = 
                varsList.[i] |> List.filter (fun (v:Var) -> bdts |> List.exists((=) (v.Type.FullName.ToLowerInvariant())) |> not)
                |> List.map 
                    (fun v -> 
                        let t = Type.Translate v.Type true None (contextList.[i])
                        new FunFormalArg<_>(t :? RefType<_> , v.Name, t))
            let nameFun:Var = ((newAST.[i]).FunVar)
            let mutable retFunType = new PrimitiveType<_>(Void) :> Type<_>
            if i <> varsList.Count-1 then
                let typeFun = newAST.[i].FunVar.Type                
                retFunType <- Type.Translate typeFun  false  None (contextList.[i])
            let typeRet = retFunType :?> PrimitiveType<_>
            let partAST, isKernel = 
                if typeRet.Type <> PTypes.Void
                then addReturn partialAstList.[i], false
                else partialAstList.[i], true

            let mainKernelFun = new FunDecl<_>(isKernel, nameFun.Name, retFunType, formalArgs,partAST)
            
            let pragmas = 
                let res = new ResizeArray<_>()
                if contextList.[i].Flags.enableAtomic
                then
                    res.Add(new CLPragma<_>(CLGlobalInt32BaseAtomics) :> TopDef<_>)
                    res.Add(new CLPragma<_>(CLLocalInt32BaseAtomics) :> TopDef<_>)
                if contextList.[i].Flags.enableFP64
                then res.Add(new CLPragma<_>(CLFP64))
                List.ofSeq res
            listCLFun <- pragmas@types@listCLFun@[mainKernelFun]
        new AST<_>(listCLFun)

    let translate qExpr translatorOptions =
        
        let structs = CollectStructs qExpr
        let context = new TargetContext<_,_>()
        let translatedStructs = Type.TransleteStructDecls structs.Keys context |> Seq.cast<_> |> List.ofSeq
        newAST <- QuotationsTransformer.quontationTransformer qExpr translatorOptions

        //let qExpr = expand Map.empty qExpr
        let rec go expr vars  =
            match expr with
            | Patterns.Lambda (v, body) -> go body (v::vars) 
            | e -> 
                let body =
                    let b,context =
                        let c = new TargetContext<_,_>()
                        c.UserDefinedTypes.AddRange context.UserDefinedTypes
                        c.UserDefinedTypesOpenCLDeclaration.Clear()
                        for x in context.UserDefinedTypesOpenCLDeclaration do c.UserDefinedTypesOpenCLDeclaration.Add (x.Key,x.Value)
                        c.Flags.enableFP64 <- context.Flags.enableFP64
                        c.Namer.LetIn()
                        c.TranslatorOptions.AddRange translatorOptions
                        vars |> List.iter (fun v -> c.Namer.AddVar v.Name)
                        //printfn "%A" e
                        Body.Translate e c
                    match b  with
                    | :? StatementBlock<Lang> as sb -> sb
                    | :? Statement<Lang> as s -> new StatementBlock<_>(new ResizeArray<_>([s]))
                    | _ -> failwithf "Incorrect function body: %A" b
                    ,context
                vars, body
            | x -> "Incorrect OpenCL quotation: " + string x |> failwith
        
        let listPartsASTVars = new ResizeArray<_>()
        let listPartsASTPartialAst = new ResizeArray<_>()
        let listPartsASTContext = new ResizeArray<_>()        
        Body.dictionaryFun.Clear()
        for partAST in  newAST do
            let vars,(partialAst,context) = go partAST.FunExpr [] 
            listPartsASTVars.Add(List.rev vars)
            listPartsASTPartialAst.Add((partialAst :> Statement<_>))
            listPartsASTContext.Add(context)
            //Body.dictionaryFun.Add(partAST.FunVar.Name, partialAst)

        let AST = buildFullAst (listPartsASTVars) translatedStructs (listPartsASTPartialAst) listPartsASTContext
        AST, newAST        
  
    member this.Translate qExpr translatorOptions = 
        let ast, newQExpr = translate qExpr translatorOptions
        ast, newQExpr