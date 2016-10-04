module Brahman.MatrixMultiply.NaiveMatrixMultiply

open Brahma.Helpers
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Extensions
open Brahma.FSharp.OpenCL.Translator.Common
open System.Threading.Tasks

let label = "OpenCL/Naive"

let command1 = 
    <@
        fun (rng : _2D) (input : array<_>) (m : int) (n : int) (k : int) (res : array<_>) ->
            let x = rng.GlobalID0
            let y = rng.GlobalID1
            let pos = x * n + y
            res.[pos] <- 0.
            for i in 0 .. n do
                res.[pos] <- res.[pos] + input.[n*x + i] * input.[m*n + i*k + y]
    @>

let command2 =
    <@
        fun (rng : _2D) (input : array<_>) (res : array<_>) ->
            let m = int (input.[0])
            let n = int (input.[1])
            let k = int (input.[2])
            res.[0] <- 0.
            ()
    @>