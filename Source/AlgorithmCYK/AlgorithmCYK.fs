module AlgorithmCYK

let unzip arr = Array.unzip(arr)
let findIndex arr elem = Array.findIndex ((=) elem) arr

let formRules (rules : array<string * string>) = 

    let separateRules (rules : array<string * string>) = 
        let termRules = ResizeArray<string * string>()
        let nontermRules = ResizeArray<string * string>()
        for i = 0 to rules.Length - 1 do
            if (snd rules.[i]).Length < 2
            then termRules.Add(rules.[i])
            else nontermRules.Add(rules.[i])
        nontermRules.ToArray(), termRules.ToArray()

    let endnRulesInTwo (endnRules : array<string>) =
        let arr1 = Array.create endnRules.Length ""
        let arr2 = Array.create endnRules.Length ""
        for i = 0 to endnRules.Length - 1 do
            arr1.[i] <- endnRules.[i].[0].ToString()
            arr2.[i] <- endnRules.[i].[1].ToString()
        arr1, arr2

    let rulesToInt (alf : array<string>) (chang : array<string>) =
        let arr = Array.zeroCreate chang.Length
        for i = 0 to alf.Length - 1 do
            for j = 0 to chang.Length - 1 do
                if chang.[j] = alf.[i]
                then arr.[j] <- i
        arr

    let (nRules, tRules) = separateRules rules
    let (begnRules, endnRules) = unzip nRules
    let (begtRules, endtRules) = unzip tRules
    let nonterm = Set.toArray (Set.ofArray(fst (unzip rules)))
    let term = Set.toArray (Set.ofArray(endtRules))
    let (endnRulesL, endnRulesR) = endnRulesInTwo endnRules

    term, nonterm, rulesToInt nonterm begnRules, (rulesToInt nonterm endnRulesL, rulesToInt nonterm endnRulesR), rulesToInt nonterm begtRules, rulesToInt term endtRules

let formStr (term : array<string>) (str : string) = 
    let arr = Array.zeroCreate str.Length
    for i = 0 to str.Length - 1 do
        for j = 0 to term.Length - 1 do
            if str.[i].ToString() = term.[j]
            then arr.[i] <- j
    arr

let rulesCheck (strEl : int) (begtRules : array<int>) (endtRules : array<int>) (mtrxel : array<int>) =
    for k = 0 to endtRules.Length - 1 do
        if endtRules.[k] = strEl
        then mtrxel.[begtRules.[k]] <- 1

let compCheck (begnRules : array<int>) (endnRulesL : array<int>, endnRulesR : array<int>) (mtrxel1 : array<int>) (mtrxel2 : array<int>) (mtrxel3 : array<int>) =
    for i = 0 to mtrxel1.Length - 1 do
        if mtrxel1.[i] = 1
        then
            for j = 0 to mtrxel2.Length - 1 do
                if mtrxel2.[j] = 1
                then 
                    for k = 0 to endnRulesL.Length - 1 do
                        if (i = endnRulesL.[k]) && (j = endnRulesR.[k])
                        then mtrxel3.[begnRules.[k]] <- 1

let matrixCYK (begtRules : array<int>) (endtRules : array<int>) (begnRules : array<int>) (endnRules : array<int>*array<int>) (str : array<int>) n (matrix : array<array<array<int>>>) = 
    for k = 0 to n - 1 do 
        rulesCheck str.[k] begtRules endtRules matrix.[n - 1].[k] 
    for l = 1 to n - 1 do
        for i = 0 to n - l - 1 do
            for j = 0 to l - 1 do
                compCheck begnRules endnRules matrix.[n - 1 - j].[i] matrix.[j + n - l].[i + j + 1] matrix.[n - 1 - l].[i]
    matrix

let conclCYK (matrix : array<array<array<int>>>) (nonterm : array<string>) start = 
    if matrix.[0].[0].[findIndex nonterm start] = 1
    then true
    else false

let CYK (rules : array<string*string>) str start = 
    if str = ""
    then true
    else 
        let (term, nonterm, begnRules, endnRules, begtRules, endtRules) = formRules rules
        let strArr = formStr term str
        let n = str.Length 
        let matrix =  [|for i in 0..n - 1 -> [|for j in 0..n - 1 -> Array.zeroCreate nonterm.Length|]|]
        let matrCYK = matrixCYK begtRules endtRules begnRules endnRules strArr n matrix
        conclCYK matrCYK nonterm start