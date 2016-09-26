module AlgorithmCYK.ParallelCPU
open AlgorithmCYK
open System.Threading.Tasks

let matrixCYKParallelCPU (begtRules : array<int>) (endtRules : array<int>) (begnRules : array<int>) (endnRules : array<int>*array<int>) (str : array<int>) n (matrix : array<array<array<int>>>) = 
    Array.Parallel.iteri(fun i arr -> rulesCheck str.[i] begtRules endtRules arr) matrix.[n - 1]
    for l = 1 to n - 1 do
        for j = 0 to l - 1 do
            Array.Parallel.iteri(fun i arr -> compCheck begnRules endnRules matrix.[n - 1 - j].[i] matrix.[j + n - l].[i + j + 1] arr) matrix.[n - l - 1].[..n - l - 1]
    matrix

let CYKParallelCPU (rules : array<string*string>) str start = 
    if str = ""
    then true
    else 
        let (term, nonterm, begnRules, endnRules, begtRules, endtRules) = formRules rules
        let strArr = formStr term str
        let n = str.Length 
        let matrix =  [|for i in 0..n - 1 -> [|for j in 0..n - 1 -> Array.zeroCreate nonterm.Length|]|]
        let matrCYK = matrixCYKParallelCPU begtRules endtRules begnRules endnRules strArr n matrix
        conclCYK matrCYK nonterm start