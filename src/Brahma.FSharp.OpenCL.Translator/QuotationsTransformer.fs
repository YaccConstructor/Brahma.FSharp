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

module Brahma.FSharp.OpenCL.Translator.QuotationsTransformer

open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Translator.Type
open Microsoft.FSharp.Collections

let mainKernelName = "brahmaKernel"

let apply (expr:Expr) =
    let rec go expr = 
        match expr with    
        | Patterns.Application (expr1,expr2) -> translateApplication expr
        | Patterns.Call (exprOpt,mInfo,args) -> 
            match exprOpt with
            | Some e -> Expr.Call(go e, mInfo, args |> List.map go)
            | None -> Expr.Call(mInfo, args |> List.map go)
        | Patterns.ForIntegerRangeLoop (i, from, _to, _do) ->
            Expr.ForIntegerRangeLoop(i, from, _to, go _do)            
        | Patterns.IfThenElse (cond, thenExpr, elseExpr) ->
            Expr.IfThenElse(go cond, go thenExpr, go elseExpr)        
        | Patterns.Let (var, expr, inExpr) ->
            Expr.Let(var, go expr, go inExpr)
        | Patterns.Sequential(expr1,expr2) -> 
            Expr.Sequential(go expr1, go expr2)        
        | Patterns.VarSet(var,expr) -> 
            Expr.VarSet(var, go expr)
        | Patterns.WhileLoop(condExpr,bodyExpr) -> 
            Expr.WhileLoop(go condExpr, go bodyExpr)
        | Patterns.Lambda(v,e) ->
            Expr.Lambda(v,go e)
        | other -> other

    and translateApplication expr =
        let rec _go expr =
            match expr with            
            | Patterns.Application (Patterns.Lambda (fv,e),v) ->
                e.Substitute(fun x -> if x = fv then Some v else None) |> _go
            | Patterns.Application (e,v) ->
                let fb = go e
                Expr.Application(fb,v) |> _go 
            | e -> e
        let body = _go expr
        body

    go expr

let inlineLamdas (expr:Expr) =
    let rec go expr = 
        match expr with
        | Patterns.Let(var, expr, inExpr) ->
            match expr with
            | Patterns.Lambda _ ->
                inExpr.Substitute(fun x -> if x = var then Some expr else None) |> go
            | Patterns.Application _ -> Expr.Let(var, go expr |> apply, inExpr) |> go
            | e -> Expr.Let(var, expr, go inExpr)
        | Patterns.Application (expr1,expr2) -> Expr.Application (go expr1 |> apply, go expr2)
        | Patterns.Call (exprOpt,mInfo,args) ->
            match exprOpt with
            | Some e -> Expr.Call(go e, mInfo, args |> List.map go)
            | None -> Expr.Call(mInfo, args |> List.map go)
        | Patterns.ForIntegerRangeLoop (i, from, _to, _do) ->
            Expr.ForIntegerRangeLoop(i, from, _to, go _do)            
        | Patterns.IfThenElse (cond, thenExpr, elseExpr) ->
            Expr.IfThenElse(go cond, go thenExpr, go elseExpr)        
        | Patterns.Sequential(expr1,expr2) -> 
            Expr.Sequential(go expr1, go expr2)        
        | Patterns.VarSet(var,expr) -> 
            Expr.VarSet(var, go expr)
        | Patterns.WhileLoop(condExpr,bodyExpr) -> 
            Expr.WhileLoop(go condExpr, go bodyExpr)            
        | other -> other
    go expr

let isLetFun expr =
    match expr with
    | Patterns.Let (var, ExprShape.ShapeLambda(lv, lb), afterExpr) -> true
    | _ -> false

let rec recLet expr = 
    match expr with
    | Patterns.Let(v, valExpr, inExpr1) ->
        match valExpr with
        | Patterns.Let (var, inExpr, afterExpr) -> 
            let newLet = Expr.Let(v, afterExpr, inExpr1) |> recLet
            Expr.Let(var, inExpr, newLet) |> letFunUp
        | ExprShape.ShapeLambda(lv, lb) ->
            let newLet = recLet valExpr
            match newLet with
            | Patterns.Let (var, inExpr, afterExpr) -> 
                let newLetIn = (Expr.Let(v, afterExpr , inExpr1))
                letFunUp (Expr.Let(var, inExpr, newLetIn))
            | _ -> Expr.Let(v, newLet , inExpr1) 
        | _ -> Expr.Let(v, recLet valExpr, inExpr1)
    | ExprShape.ShapeVar var -> expr           
    | ExprShape.ShapeLambda(lv, lb1) ->
        match lb1 with
        | ExprShape.ShapeLambda(lv1, lb) ->
            let lb = recLet lb1
            match lb with
            | Patterns.Let (var, inExpr, afterExpr) when isLetFun lb ->
                Expr.Let(var, inExpr, (Expr.Lambda(lv, afterExpr)))                
            | _ -> Expr.Lambda(lv, lb)
        | _ -> 
            let funUpLB = letFunUp lb1
            match funUpLB with
            | Patterns.Let (var, inExpr, afterExpr) when isLetFun funUpLB ->                
                Expr.Let(var, inExpr, (Expr.Lambda(lv, afterExpr)))
            | _ -> Expr.Lambda(lv, funUpLB)

    | ExprShape.ShapeCombination(o, args) ->
        ExprShape.RebuildShapeCombination(o, List.map (fun (e:Expr) -> recLet e) args)

and letFunUp expr =
    match expr with
    | Patterns.Let(var, inExpr, body) ->
        let recResOutLetFun = recLet expr
        match recResOutLetFun with
        | Patterns.Let(v, iE, b) ->
            let retFunUp = letFunUp b
            if isLetFun recResOutLetFun
            then Expr.Let(v, iE, retFunUp)
            else
                match retFunUp with
                | Patterns.Let(vF, iEF, bF) when isLetFun retFunUp ->                    
                    let newAfterExpr = Expr.Let(v, iE, bF)
                    Expr.Let(vF, iEF, letFunUp newAfterExpr)
                | Patterns.Let(vF, iEF, bF) ->
                    let fUp = retFunUp
                    match fUp with
                    | Patterns.Let (var2, inExpr2, afterExpr2) when isLetFun fUp ->
                        let newAfterExpr = Expr.Let(v, iE, afterExpr2)
                        Expr.Let(var2, inExpr2, letFunUp newAfterExpr)
                    | _ -> Expr.Let (v, iE, fUp)
                | _ -> recResOutLetFun
        | _ -> recResOutLetFun

    | Patterns.IfThenElse (cond, thenExpr, elseExpr) ->
        let newCond = cond
        let newThenExpr = letFunUp thenExpr
        match newThenExpr with
        | Patterns.Let (var2, inExpr2, afterExpr2) ->
            if isLetFun newThenExpr
            then
                let newAfterExpr = Expr.IfThenElse(cond, afterExpr2, elseExpr)
                Expr.Let(var2, inExpr2, recLet newAfterExpr)
            else 
                let newElseExpr = letFunUp elseExpr
                match newElseExpr with
                | Patterns.Let (var1, inExpr1, afterExpr1) when isLetFun newElseExpr ->
                    let newAfterExpr = Expr.IfThenElse(cond, thenExpr, afterExpr1)
                    Expr.Let(var1, inExpr1, recLet newAfterExpr)                    
                | _ -> Expr.IfThenElse(cond, thenExpr, elseExpr)
        | _ -> 
            let newElseExpr = letFunUp elseExpr
            match newElseExpr with
            | Patterns.Let (var1, inExpr1, afterExpr1) when isLetFun newElseExpr ->                
                let newAfterExpr = Expr.IfThenElse(cond, thenExpr, afterExpr1)
                Expr.Let(var1, inExpr1, recLet newAfterExpr)                
            | _ -> Expr.IfThenElse(cond, thenExpr,elseExpr)

    | Patterns.ForIntegerRangeLoop(v, f, t, body) ->
        let newBody = letFunUp body
        match newBody with
        | Patterns.Let (var2, inExpr2, afterExpr2) when isLetFun newBody ->
            let newAfterExpr = Expr.ForIntegerRangeLoop(v, f, t, afterExpr2)
            Expr.Let(var2, inExpr2, recLet newAfterExpr)
        | _ -> Expr.ForIntegerRangeLoop(v, f, t, newBody)

    | Patterns.WhileLoop(cond, body) ->
        let newBody = letFunUp body
        match newBody with
        | Patterns.Let (var2, inExpr2, afterExpr2) when isLetFun newBody ->
            let newAfterExpr = Expr.WhileLoop(cond, afterExpr2)
            Expr.Let(var2, inExpr2, recLet newAfterExpr)
        | _ -> Expr.WhileLoop(cond, newBody)

    | _ -> expr

let rec transform expr = 
    match expr with
    | ExprShape.ShapeLambda(lv, lb) ->
        Expr.Lambda(lv, transform lb)
    | _ -> letFunUp expr

let renameTree expr = 
    let renamer = new Namer()
    let rec renameRec expr =
        match expr with
        | Patterns.Lambda (param, body) ->
            let newName = renamer.GetUnicName param.Name
            let newVar = new Var (newName, param.Type, param.IsMutable)                        
            let newBody = 
                body.Substitute (fun v -> if v = param then Some (Expr.Var newVar) else None)
                |> renameRec
            Expr.Lambda(newVar,newBody)

        | Patterns.Let(var, expr1, expr2) ->
            let newName = renamer.GetUnicName var.Name
            let newVar = new Var(newName, var.Type, var.IsMutable)
            let exprIn = renameRec expr1
            let exprAfter = 
                expr2.Substitute(fun v -> if v = var then Some (Expr.Var newVar) else None)
                |> renameRec 
            let newLet = Expr.Let(newVar, exprIn, exprAfter)
            newLet

        | Patterns.ForIntegerRangeLoop (i, from, _to, _do) ->
            let newName = renamer.GetUnicName (i.Name)
            let newVar = new Var(newName, i.Type, i.IsMutable)
            let newFrom = renameRec from
            let newTo = renameRec _to
            let newDo =
                _do.Substitute(fun v -> if v = i then Some (Expr.Var newVar) else None)
                |> renameRec 

            Expr.ForIntegerRangeLoop(newVar, newFrom, newTo, newDo)
                                
        | ExprShape.ShapeCombination (o, exprs) -> ExprShape.RebuildShapeCombination (o, List.map renameRec exprs)

        | x -> x

    let rec quontationRenamerLetRec expr =
        match expr with
        | ExprShape.ShapeLambda(lv, lb) ->
            let newName = renamer.GetUnicName (lv.Name)
            let newVar = new Var(newName, lv.Type, lv.IsMutable)
            let newLambda = 
                Expr.Lambda(newVar, quontationRenamerLetRec (lb.Substitute(fun v -> if v = lv then Some (Expr.Var newVar) else None)))
            newLambda            
        | _ ->
            renameRec expr
    quontationRenamerLetRec expr

let letToExtend = new System.Collections.Generic.Dictionary<_,_>()
let lets = new System.Collections.Generic.HashSet<_>()
let addNeededLamAndAppicatins (expr:Expr) =
    let globalFree = new ResizeArray<_>()
    globalFree.AddRange <| expr.GetFreeVars()
    let rec addNeededLam (expr:Expr) =
        match expr with
        | ExprShape.ShapeVar var ->
            if letToExtend.ContainsKey var
            then
                let neededVars,newVar = letToExtend.[var]
                if neededVars |> List.length > 0
                then                    
                    neededVars
                    |> List.fold 
                        (fun ready (elem:Var) ->
                                Expr.Application(ready, Expr.Var elem))
                        (Expr.Var newVar)
                else expr
             else  expr          

        | Patterns.Let(var, expr1, expr2) ->
            lets.Add var |> ignore
            let neededVars =
                expr1.GetFreeVars() |> List.ofSeq
                |> List.filter (fun v -> not <| letToExtend.ContainsKey v && not <| lets.Contains v)
            if neededVars.Length > 0 && isLetFun expr
            then
                let readyLet =
                    neededVars 
                    |> List.rev
                    |> List.fold
                        (fun readyLet elem -> Expr.Lambda(elem, readyLet))
                        (addNeededLam expr1)
                let newVar = new Var(var.Name, readyLet.Type)
                letToExtend.Add(var, (neededVars, newVar))
                Expr.Let(newVar, readyLet, addNeededLam expr2)
            else
                let ex1 = addNeededLam expr1
                let ex2 = addNeededLam expr2
                let newLet = Expr.Let(var, ex1, ex2)
                newLet

        | Patterns.Application(expr1, expr2) ->
            let exp1 = addNeededLam expr1
            let exp2 = addNeededLam expr2
            let readyApp = Expr.Application(exp1, exp2)
            readyApp

        | ExprShape.ShapeLambda(lv, lb) ->
            let newExpr = addNeededLam lb
            Expr.Lambda(lv, newExpr)

        | ExprShape.ShapeCombination(o, args) ->
            ExprShape.RebuildShapeCombination(o, List.map (fun e -> addNeededLam e) args)

    let rec run expr =
        match expr with
        | ExprShape.ShapeLambda(lv, lb) ->
            Expr.Lambda(lv, run lb)
        | _ ->         
            addNeededLam expr
    run expr

let getListLet expr =
    let namer = new Namer()
    let listExpr = new ResizeArray<_>()
    let rec addLetInList expr =
        match expr with
        | Patterns.Let(var, exprIn, exprAfter) when isLetFun expr ->                        
            let newLet = Expr.Let(var, exprIn, Expr.Value(0)) // in  body some value
            listExpr.Add newLet
            addLetInList exprAfter            
        | _ -> expr

    and firstLams expr =
        match expr with
        | Patterns.Lambda(lv, lb) -> Expr.Lambda(lv, firstLams lb)
        | _ -> addLetInList expr
        
    match expr with
    | Patterns.Lambda(lv, lb) -> listExpr.Add(firstLams (Expr.Lambda(lv, lb)))
    | _ -> ()
    
    listExpr  
    |> ResizeArray.map
       (fun elem ->
            match elem with
            | Patterns.Let(v, e, b) ->
                new Method(v,e)
            | Patterns.Lambda(lv, lb) ->
                let newVar = new Var(mainKernelName, lv.Type, false)
                new Method(newVar, elem)            
            | x -> failwithf "Anexpected element: %A" x
       )

let quontationTransformer expr translatorOptions =
    let renamedTree = renameTree expr
    let qTransformed = transform renamedTree    
    let addedLam =
        addNeededLamAndAppicatins qTransformed 
        |> (fun x ->
                lets.Clear()
                letToExtend.Clear()
                addNeededLamAndAppicatins x)

    let listExpr = getListLet addedLam
    listExpr