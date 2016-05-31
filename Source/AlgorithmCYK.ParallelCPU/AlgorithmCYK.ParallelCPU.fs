module AlgorithmCYK.ParallelCPU
open AlgorithmCYK
open System.Threading.Tasks

let matrixCYKParallelCPU (rules : list<string * string>) (nonterm : string array) (str : string) (n : int) (matrix : int array [][]) = 
    Array.Parallel.iteri (fun i arr -> allRulesCheck rules nonterm arr [|(str.[i].ToString())|]) matrix.[n - 1]
    for l = 1 to n - 1 do
        for j = 0 to l - 1 do
            Array.Parallel.iteri(fun i arr -> allRulesCheck rules nonterm arr (compCreate nonterm matrix.[n - 1 - j].[i] matrix.[j + n - l].[i + j + 1])) matrix.[n - 1 - l].[..n - l - 1]
    matrix

let CYKParallelCPU (rules : list<string * string>) (nonterm : string array) (str : string) (start : string) = 
    if str = ""
    then true
    else 
        let n = str.Length 
        let matrix =  [|for i in 0..n - 1 -> [|for j in 0..n - 1 -> Array.zeroCreate nonterm.Length|]|]
        let matrCYK = matrixCYKParallelCPU rules nonterm str n matrix
        conclCYK matrCYK nonterm start