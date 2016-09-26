module ArrayGPU.TimeTests
open FSharp.Charting
open System.Drawing
open System.Windows.Forms


let diagram f i = 
    let arr1 = [|for j in 1..i -> j|]
    let time1 = System.DateTime.Now
    let t = ArrayGPU.init arr1
    let time2 = System.DateTime.Now
    let outarr = f arr1 t
    let time3 = System.DateTime.Now
    let outarr = ArrayGPU.getResult outarr t
    let time4 = System.DateTime.Now
    let d = Chart.Pie ["initialization", (time2 - time1).TotalMilliseconds; "function", (time3 - time2).TotalMilliseconds; "getting result", (time4 - time3).TotalMilliseconds]
    do System.Windows.Forms.Application.Run (d.ShowChart())

diagram (ArrayGPU.Map <@ fun x -> x + 1 @>) 10000000 |> ignore


let timer1 f i n = //for Array and Array.Parallel
    if i = 0 then 0.00
    elif i > 10000000 then 0.00
    else
        let arr1 = [|for j in 1..i -> j|]
        //let arr2 = [|for j in 1..i -> j|]
        let time f =
            let beg = System.DateTime.Now
            for i in 1..n do f() |> ignore 
            ((System.DateTime.Now - beg).TotalMilliseconds) / float n
        let res = (time (fun() -> f arr1 (*arr2*)))
        res

let timer2 f i n = //for ArrayGPU
    if i = 0 then 0.00
    elif i > 10000000 then 0.00
    else
        let arr1 = [|for j in 1..i -> j|]
        //let arr2 = [|for j in 1..i -> j|]
        let t = ArrayGPU.init arr1 
        let time f =
            let beg = System.DateTime.Now
            for i in 1..n do f() |> ignore 
            ((System.DateTime.Now - beg).TotalMilliseconds)/ float n
        let res = (time (fun() -> f arr1 (*arr2*)  t))
        ArrayGPU.getResult (f arr1 (*arr2*) t) t |>ignore
        res
 
let n = 3
do timer2 (ArrayGPU.Map <@ fun x -> sin(float(x)) @>) 100 n |> ignore
let main lowBound skip highBound = 
    Chart.Combine (
        [Chart.Line ([for i in lowBound..skip..highBound -> (i, timer1 (Array.map (fun x -> sin(float(x)))) i n)], Name = "Array", Color = System.Drawing.Color.Green)
         Chart.Line ([for i in lowBound..skip..highBound -> (i, timer1 (Array.Parallel.map (fun x -> sin(float(x)))) i n)], Name = "Parallel", Color = System.Drawing.Color.Blue)
         Chart.Line ([for i in lowBound..skip..highBound -> (i, timer2 (ArrayGPU.Map <@ fun x -> sin(float(x)) @>) i n)], Name = "GPU", Color = System.Drawing.Color.Red)]
         )
do System.Windows.Forms.Application.Run ((main 500000 500000 10000000).ShowChart())

