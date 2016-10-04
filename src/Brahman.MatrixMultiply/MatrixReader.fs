module Brahman.MatrixMultiply.MatrixReader

let readFromFile filename m n k bufs =
    Some bufs

let mutable counter = 1

let generate m n k (bufs : array<float>) =
    if counter = 0
    then None
    else
        let r = new System.Random(100)
        for i in 0 .. m-1 do
            for j in 0 .. n-1 do
                bufs.[n*i+j] <- r.NextDouble() * 10.
        for i in 0 .. n-1 do
            for j in 0 .. k-1 do
                bufs.[m*n + k*i+j] <- r.NextDouble() * 10.
        counter <- counter - 1  
        Some bufs