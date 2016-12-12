module AgentsExamples

open Brahma.Helpers
open OpenCL.Net
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Extensions
open Brahma.FSharp.Agents

//Following function returns array of integer filled with elements of the numeric row.
//Each new element of the row is calculated by the following formula:
// x(n) = n ^ 4 + n ^ 2 + 1 if n is odd
// x(n) = n ^ 5 + n ^ 3 + n if n is even
// n = {0, 1, 2, ..., l}
let working l =    
    let rec pow (a, b) =
        match b with 
        | 0 -> 1
        | 1 -> a
        | _ -> match b % 2 with
                  | 1 -> pow(a, b / 2) * pow(a, b / 2) * a
                  | 0 -> pow(a, b / 2) * pow(a, b / 2)
                  | _ -> 1
    
    let fun1 x = pow(x, 4) + pow(x, 2) + 1
    let fun2 x = pow(x, 5) + pow(x, 3) + x
    
    let even x = 2 * x + 1
    let odd x = 2 * x
    
    let arr = Array.init (l) (fun i -> i)
    
    let computeArr1 i = arr.[i] <- fun1 arr.[i]
    let computeArr2 i = arr.[i] <- fun2 arr.[i]
    
    let workerEven = new Worker<int, int>(even)
    let workerOdd = new Worker<int, int>(odd)
    for i  in 0 .. l / 2 - 1 do 
        workerEven.Process (i, computeArr1)
        workerOdd.Process (i, computeArr2)
    workerEven.Die()
    arr

