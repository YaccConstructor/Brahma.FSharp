module PerformanceCYK

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Collections

open Brahma.OpenCL

type tblData = uint64
type coordinate = uint32
type symbol = uint16 

[<Struct>]
type CellData =
    val mutable rData : tblData
    val mutable _k : coordinate
    new (r, k) = {rData=r;_k=k}

let command = 
    <@
        fun (grid:_2D) (table:CellData[]) (rules:uint64[]) (*(tableLenNum:_[])*) (rowSizeNum:_[]) (nTermsCountNum:_[]) (lNum:_[]) (indexesBySymbols:int[]) (biggestSymNum:_[])-> 
            let i = grid.GlobalID0 // column index, may be accessed parallel
            let s = grid.GlobalID1 // symbol index
            let l = lNum.[0] // row index, must be accessed sequentially
            let rowSize = rowSizeNum.[0]
            // check is this cell in needed matrix part
            if (i <= (rowSize - 1 - l)) then
                let nTermsCount = nTermsCountNum.[0] 
                let biggestSym = biggestSymNum.[0]
                let symbolStart = s * biggestSym
                let symbolEnd = s * biggestSym + biggestSym - 1

                let noLbl = 0uy
                let maxValue = System.UInt64.MaxValue
                    
                for r in symbolStart..symbolEnd do
                    let ruleIndex = indexesBySymbols.[r]
                    if ruleIndex >= 0 then
                        let currentRule = rules.[ruleIndex]
                        
                        for k in 0..(l - 1) do                                                    
                            (* process rule *)
                            (* get r2 *)
                            let r2 = Microsoft.FSharp.Core.Operators.uint32 (currentRule &&& 0xFFFFFFFFUL)
                            let r2 = Microsoft.FSharp.Core.Operators.uint16 ((r2 >>> 16) &&& 0xFFFFFFFFu)
                            if r2 <> 0us then
                                let index = k
                                let leftDiff = index * (index - 1) / 2
                                let leftStart = ( k * rowSize + i - leftDiff ) * nTermsCount
                                let index = l-k-1
                                let rightDiff = index * (index - 1) / 2 
                                let rightStart = ( (l-k-1) * rowSize + k+i+1 - rightDiff ) * nTermsCount
                                for m in 0..nTermsCount - 1 do
                                    let leftCell:CellData = table.[leftStart + m]
                                    (* get r1 *)
                                    let r1 = Microsoft.FSharp.Core.Operators.uint32 ((currentRule >>> 32) &&&  0xFFFFFFFFUL)
                                    let r1 = Microsoft.FSharp.Core.Operators.uint16 (r1 &&& 0xFFFFFFFFu)
                                    let isLeftEmpty = leftCell.rData = maxValue && leftCell._k = 0ul
//                                    ()
//                                    ()
                                    if not isLeftEmpty then
//                                        (* get rule num *)
                                        let leftRuleNum = int ((leftCell.rData >>> 32) &&&  0xFFFFFFFFUL)                                      
//                                        (* get rule name *)
                                        let leftRuleNamePart = Microsoft.FSharp.Core.Operators.uint32 ((rules.[leftRuleNum] >>> 32) &&&  0xFFFFFFFFUL)
//                                        (* get cell rule top *)
                                        let leftTop = Microsoft.FSharp.Core.Operators.uint16 ((leftRuleNamePart >>> 16) &&& 0xFFFFFFFFu)
//                                        (* is cell data empty *)
                                        if leftTop = r1 then
                                            for n in 0..nTermsCount - 1 do
                                                let rightCell = table.[rightStart + n]
                                                let isRightEmpty = rightCell.rData = maxValue && rightCell._k = 0ul 
                                                if not isRightEmpty then
                                                    (* get rule num *)
                                                    let rightRuleNum = int (((*rightCell.rData*)1UL >>> 32) &&&  0xFFFFFFFFUL)                          
                                                    (* get rule name *)
                                                    let rightRuleNamePart = Microsoft.FSharp.Core.Operators.uint32 ((rules.[rightRuleNum] >>> 32) &&&  0xFFFFFFFFUL)
//                                                    (* get cell rule top *)
                                                    let rightTop = Microsoft.FSharp.Core.Operators.uint16 ((rightRuleNamePart >>> 16) &&& 0xFFFFFFFFu)
//                                                    (* is cell data empty *)             
                                                    if rightTop = r2 then
//                                                    (* get rule label *)
                                                        let buf = Microsoft.FSharp.Core.Operators.uint16 (Microsoft.FSharp.Core.Operators.uint32 (currentRule &&& 0xFFFFFFFFUL) &&& 0xFFFFFFFFu)
                                                        let label = Microsoft.FSharp.Core.Operators.byte ((buf >>> 8) &&& Microsoft.FSharp.Core.Operators.uint16 0xFFFFFFFFu)
                                                        (* get cell label *)
                                                        let buf = Microsoft.FSharp.Core.Operators.uint32 (leftCell.rData &&& 0xFFFFFFFFUL)
                                                        let lbl = Microsoft.FSharp.Core.Operators.uint16 (buf &&& 0xFFFFFFFFu)
                                                        let label1 = Microsoft.FSharp.Core.Operators.byte ((lbl >>> 8) &&& 0xFFus)
                                                        (* get label state *)
                                                        let buf = Microsoft.FSharp.Core.Operators.uint32 (leftCell.rData &&& 0xFFFFFFFFUL)
                                                        let lState1Part = Microsoft.FSharp.Core.Operators.uint16 ((buf >>> 16) &&& 0xFFFFFFFFu)
                                                        let lState1 = Microsoft.FSharp.Core.Operators.uint32 lState1Part
                                                        (* get cell label *)
                                                        let buf = Microsoft.FSharp.Core.Operators.uint32 (rightCell.rData &&& 0xFFFFFFFFUL)
                                                        let lbl = Microsoft.FSharp.Core.Operators.uint16 (buf &&& 0xFFFFFFFFu)
                                                        let label2 = Microsoft.FSharp.Core.Operators.byte ((lbl >>> 8) &&& 0xFFus)
                                                        (* get label state *)
                                                        let buf = Microsoft.FSharp.Core.Operators.uint32 (rightCell.rData &&& 0xFFFFFFFFUL)
                                                        let lState2Part = Microsoft.FSharp.Core.Operators.uint16 ((buf >>> 16) &&& 0xFFFFFFFFu)
                                                        let lState2 = Microsoft.FSharp.Core.Operators.uint32 lState2Part
                                                        (* label states *)
                                                        let sDefined = 0u
                                                        let sUndefined = 1u
                                                        let sConflict = 2u
                                                
                                                        let newLabel = ref noLbl
                                                        let newState = ref sUndefined                                             
                                                        (* choose new label *)
                                                        if lState1 = sConflict then newLabel := noLbl//; newState := sConflict
                                                        elif lState2 = sConflict then newLabel := noLbl//; newState := sConflict
                                                        elif lState1 = sUndefined && lState2 = sUndefined && label = noLbl 
                                                        then newLabel := noLbl//; newState := sUndefined
                                                        else
                                                            let mutable notEmptyLbl1 = noLbl
                                                            let mutable notEmptyLbl2 = noLbl
                                                            let mutable notEmptyLbl3 = noLbl 
                                                            let mutable realLblCount = 0
                                                            if label1 <> noLbl then 
                                                                notEmptyLbl1 <- label1
                                                                realLblCount <- realLblCount + 1
                                                            if label2 <> noLbl then
                                                                if realLblCount = 0 then notEmptyLbl1 <- label2
                                                                elif realLblCount = 1 then notEmptyLbl2 <- label2
                                                                realLblCount <- realLblCount + 1
                                                            if label <> noLbl then 
                                                                if realLblCount = 0 then notEmptyLbl1 <- label
                                                                elif realLblCount = 1 then notEmptyLbl2 <- label
                                                                elif realLblCount = 2 then notEmptyLbl3 <- label
                                                                realLblCount <- realLblCount + 1
                                                            if realLblCount = 1 ||
                                                                (realLblCount = 2 && notEmptyLbl2 = notEmptyLbl1) ||
                                                                (realLblCount = 3 && notEmptyLbl2 = notEmptyLbl1 && notEmptyLbl3 = notEmptyLbl1)
                                                            then newLabel := notEmptyLbl1//; newState := sDefined
                                                            else newLabel := noLbl//; newState := sConflict
                                                
                                                        (* get rule weight *)
                                                        let buf = Microsoft.FSharp.Core.Operators.uint16 (Microsoft.FSharp.Core.Operators.uint32 (currentRule &&& 0xFFFFFFFFUL) &&& 0xFFFFFFFFu)
                                                        let weight = Microsoft.FSharp.Core.Operators.byte (buf &&& Microsoft.FSharp.Core.Operators.uint16 0xFFFFFFFFu)
                                                        (* get cell weight *)
                                                        let buf = Microsoft.FSharp.Core.Operators.uint32 (leftCell.rData &&& 0xFFFFFFFFUL)
                                                        let lbl = Microsoft.FSharp.Core.Operators.uint16 (buf &&& 0xFFFFFFFFu)
                                                        let weight1 = Microsoft.FSharp.Core.Operators.byte (lbl &&& 0xFFFFus)
                                                        (* get cell weight *)
                                                        let buf = Microsoft.FSharp.Core.Operators.uint32 (rightCell.rData &&& 0xFFFFFFFFUL)
                                                        let lbl = Microsoft.FSharp.Core.Operators.uint16 (buf &&& 0xFFFFFFFFu)
                                                        let weight2 = Microsoft.FSharp.Core.Operators.byte (lbl &&& 0xFFFFus)
                                                        let newWeight = weight + weight1 + weight2
                                                
                                                        (* build data *)
                                                        let currentLabel = 
                                                            (Microsoft.FSharp.Core.Operators.uint16 !newLabel <<< 8) 
                                                            ||| Microsoft.FSharp.Core.Operators.uint16 newWeight
                                                        let currentR2 = (Microsoft.FSharp.Core.Operators.uint32 !newState <<< 16) ||| Microsoft.FSharp.Core.Operators.uint32 currentLabel
                                                        let currentElem = (uint64 ruleIndex <<< 32) ||| uint64 currentR2

                                                        (* get rule name *)
                                                        let ruleNamePart = Microsoft.FSharp.Core.Operators.uint32 ((currentRule >>> 32) &&&  0xFFFFFFFFUL)
                                                        (* get cell rule top *)
                                                        let ruleName = int (Microsoft.FSharp.Core.Operators.uint16 ((ruleNamePart >>> 16) &&& 0xFFFFFFFFu))
                                                        let diff = l * (l - 1) / 2
                                                        let index = ( l * rowSize + i - diff ) * nTermsCount + ruleName - 1  
                                                        ()                                                 
                                                        //table.[index].rData <- currentElem
                                                        //table.[index]._k <- Microsoft.FSharp.Core.Operators.uint32 k
                    
    @>


let isLetFun expr =
    match expr with
    | Patterns.Let (var, ExprShape.ShapeLambda(lv, lb), afterExpr) -> true
    | _ -> false

let rec recLet expr = 
    match expr with
    | Patterns.Let(v, valExpr, inExpr1) ->
        match valExpr with
        | Patterns.Let (v, e, afterExpr) -> 
            let newLet = Expr.Let(v, afterExpr, inExpr1) |> recLet
            Expr.Let(v, e, newLet) |> letFunUp
        | ExprShape.ShapeLambda(lv, lb) ->
            let newLet = recLet valExpr
            match newLet with
            | Patterns.Let (var, inExpr, afterExpr) -> 
                let newLetIn = Expr.Let(v, afterExpr , inExpr1)
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
    //printfn "%A" (hash expr)
    match expr with
    | Patterns.Let(_, _, _) ->
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
                    let fUp = (*letFunUp*) retFunUp
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

    | _ -> (*printfn "F";*) expr

let rec transform expr = 
    match expr with
    | ExprShape.ShapeLambda(lv, lb) ->
        Expr.Lambda(lv, transform lb)
    | _ -> letFunUp expr

let answer() = 
    let sw = System.Diagnostics.Stopwatch()
    sw.Start()
    transform command
    |> ignore
    sw.Stop()
    printfn "Time elapsed: %A" <| sw.Elapsed