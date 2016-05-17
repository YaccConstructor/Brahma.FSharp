module AlgorithmCYK
open System

let rulesInput = 
    let array = [||]
    printfn "%s" "Enter a context-free grammar in Chomsky normal form: \n"
    let rec rulesInput array =
        let mutable arr = array
        printfn "%s" "Enter a left part of the rule:  "
        let lp = Console.ReadLine()
        printfn "%s" "Enter a right part of the rule:  "
        let rp = Console.ReadLine()
        arr <- Array.append arr  [|(lp,rp)|]
        printfn "%s" "Press ENTER to continue or SPACE to end."
        let rk = Console.ReadKey()
        if (rk.Key).ToString() = "Spacebar"
        then arr
        else rulesInput arr
    rulesInput array

let oneRuleCheck ((ch : string),(str: string)) (comp : string) =
    if comp = str 
    then [|ch|]
    else [||]

let allRulesCheck (rules : array<string * string>) (comp : string array) = 
    let len = Array.length(comp)
    let mutable arr = [||]
    let n = Array.length(rules)
    for j in 0..len - 1 do
        for i in 0..n - 1 do
           arr <- Array.append arr (oneRuleCheck rules.[i] comp.[j])
    arr

let deleteEqual (arr : array<'t>) = 
    if arr <> [||] 
    then 
        let n = Array.length(arr)
        let mutable res = [|arr.[0]|]
        for i = 1  to n - 1 do
            let mutable f = 0
            for j = 0 to Array.length(res) - 1 do
                if arr.[i] = res.[j]
                then f <- 1
            if f = 0
            then res <- Array.append res [|arr.[i]|]
        res
    else arr

let decMultiply (arr1 : array<string>) (arr2 : array<string>) =
    let mutable res = [||]
    let n1 = Array.length(arr1)
    let n2 = Array.length(arr2)
    match n1, n2 with
    | 0, n2 -> arr2
    | n1, 0 -> arr1
    | _, _ -> for i in 0..n1 - 1 do
                  for j in 0..n2 - 1 do
                      res <- Array.append res [|arr1.[i] + arr2.[j]|]
              res

let matrixCYK (rules : array<string * string>) (str: string) (n: int) (matrix  : string array [][]) = 
    let mutable dm = [||] 
    for i = 0 to n - 1 do 
        matrix.[n - 1].[i] <- allRulesCheck rules [|(str.[i].ToString())|]
    for l = 1 to n - 1 do
        for i = 0 to n - l - 1 do
            for j = 0 to l - 1 do
                dm <- decMultiply matrix.[n - 1 - j].[i] matrix.[j + n - l].[i + j + 1]
                matrix.[n - 1 - l].[i]  <- deleteEqual (Array.append matrix.[n - 1 - l].[i] (allRulesCheck rules dm))
    matrix
 
let mainCYK (rul : (string*string) []) (s: string) = 
    let mutable rules = [||]
    if rul = [||]
    then 
        rules <- rulesInput
    else 
        rules <- rul
    printfn "%A" rules
    let mutable str = ""
    printfn "%s" "Enter a string for the CYK algorithm: \n"
    if s = ""
    then
         str <- Console.ReadLine()
    else 
        str <- s
    if String.length(str) > 1
    then 
        let n = String.length(str) 
        let matrix  : string array [][] =  [| for i in 0 .. n - 1 do yield [| for j in 0 .. n - 1 do yield [|""|] |] |]
        let m = matrixCYK rules str n matrix
        let mutable f = 0
        for i = 0 to Array.length(m.[0].[0]) - 1 do
            if m.[0].[0].[i] = "S"
            then f <- 1
        if f = 1 
        then "The string can be generated. "
        else "The string cannot be generated. "
    else "The string's lenght < 1. "

printfn "%s" (mainCYK [||] "")

let rk = Console.ReadKey(true)