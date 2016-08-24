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

module BlackScholes

open Brahma.Helpers
open OpenCL.Net
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Extensions
open FSharp.Quotations.Evaluator

let main () =
    let Eps = 0.001f
        
    let riskFreeInterestRate = 0.02f
    let volatility = 0.30f

    let RiskFreeInterestRate = riskFreeInterestRate
    let Volatility = volatility

    let A1 = 0.31938153f
    let A2 =  -0.356563782f
    let A3 = 1.781477937f
    let A4 =  -1.821255978f
    let A5 = 1.330274429f

    let cumulativeNormalDistribution = 
        let k = <@ fun x -> 1.0f / (1.0f + 0.2316419f * (abs x)) @>
        let cnd = 
            <@ fun x ->          
                    1.0f - 1.0f / sqrt(2.0f * float32 System.Math.PI)
                    * exp (-(abs x) * (abs x) / 2.0f) * (A1 * (%k) x + A2 *(%k) x * (%k) x + A3 * (pown ((%k) x) 3)
                    + A4 * (pown ((%k) x) 4) + A5 * (pown ((%k) x) 5))            
            @>

        <@fun x -> if x < 0.0f then 1.0f - (%cnd) x else (%cnd) x@>

    let d1 =
        <@
            fun stockPrice strikePrice timeToExpirationYears ->
                log(stockPrice / strikePrice) + (riskFreeInterestRate + volatility * volatility / 2.0f) * timeToExpirationYears
                  / (volatility * sqrt timeToExpirationYears)
        @>

    let D1 = d1.Compile()

    let d2 =
        <@
            fun d1 timeToExpirationYears ->
                d1 - volatility * sqrt timeToExpirationYears
        @>

    let D2 = d2.Compile()

    let blackScholesCallOption =
        <@
            fun d1 d2 stockPrice strikePrice timeToExpirationYears ->
                stockPrice * (%cumulativeNormalDistribution) d1 -
                strikePrice * exp(-(riskFreeInterestRate) * timeToExpirationYears) * (%cumulativeNormalDistribution) d2
        @>

    let BlackScholesCallOption = blackScholesCallOption.Compile()

    let blackScholesPutOption = 
        <@
            fun d1 d2 stockPrice strikePrice timeToExpirationYears ->
                strikePrice * exp(-riskFreeInterestRate * timeToExpirationYears) * (%cumulativeNormalDistribution)(-d2) - stockPrice * (%cumulativeNormalDistribution)(-d1)        
        @>

    let BlackScholesPutOption = blackScholesPutOption.Compile()
    
    let Main() =
        let platformName = "nvidia*"
        let iterations = 1
        let optionCount = 40000
        let localWorkSize = 32
        let deviceType = DeviceType.Default

    
        let provider =
            try  ComputeProvider.Create(platformName, deviceType)
            with 
            | ex -> failwith ex.Message

        printfn "Using %A" provider

        let commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)

        let random = new System.Random(2013)

        let putCpu = Array.init  optionCount (fun _ -> -1.0f)
        let callCpu = Array.init  optionCount (fun _ -> -1.0f)
            
        let putParallel = Array.copy putCpu
        let callParallel = Array.copy callCpu

        let stockPrices = Array.init optionCount (fun _ -> random.Random(5.0f, 30.0f))
        let strikePrices = Array.init optionCount (fun _ -> random.Random(1.0f, 100.0f))
        let timesToExpiration = Array.init optionCount (fun _ -> random.Random(0.25f, 10.0f))
    
        printfn "Running %A iterations on %A options using .NET..." iterations optionCount

        for iteration in 0..iterations do
            Timer<string>.Global.Start()
            for i in 0..optionCount-1 do
                let d1 = D1 stockPrices.[i] strikePrices.[i] timesToExpiration.[i]
                let d2 = D2 d1 timesToExpiration.[i]
                putCpu.[i] <- BlackScholesPutOption d1 d2 stockPrices.[i] strikePrices.[i] timesToExpiration.[i]                                                
                callCpu.[i] <- BlackScholesCallOption d1 d2 stockPrices.[i] strikePrices.[i] timesToExpiration.[i]                                                

            Timer<string>.Global.Lap(".NET", true)    
            
        printfn "done"

        printfn "Compiling kernel(s)..."

        let blackScholes = 
            <@ fun (rng:_1D) (stocks:array<_>) (strikes:array<_>) (times:array<_>) (call:array<_>) (put:array<_>) ->            
                let index = rng.GlobalID0            

                let d1 = (%d1) stocks.[index] strikes.[index] times.[index]
                let d2 = (%d2) d1 times.[index]
            
                call.[index] <- (%blackScholesCallOption) d1 d2 stocks.[index] strikes.[index] times.[index]
                put.[index] <- (%blackScholesPutOption) d1 d2 stocks.[index] strikes.[index] times.[index]
             @>

        printfn "done."

        printfn "Running %A iterations on %A options using OpenCL..." iterations optionCount

        let c =  string blackScholes

        let kernel, kernelPrepare, kernelRun = provider.Compile blackScholes

        Timer<string>.Global.Start(); // We're going to time the OpenCL version a little differently
        kernelPrepare (new _1D(optionCount, localWorkSize)) stockPrices strikePrices timesToExpiration callParallel putParallel
        for iteration in 0..iterations do    
            commandQueue.Add(kernelRun()).Finish() |> ignore

        commandQueue.Finish() |> ignore
        Timer<string>.Global.Lap("OpenCL", true);

        printfn "done"

        printfn "Verifying results..."
        commandQueue
            .Add(putParallel.ToHost provider)
            .Add(callParallel.ToHost provider)
            .Finish()
            |> ignore

        let mutable isSuccess1 = true
        let mutable isSuccess2 = true

        for i in 0..optionCount do
            if isSuccess1 && abs (putCpu.[i] - putParallel.[i]) > Eps
            then
                isSuccess1 <- false
                printfn "Put. Expected: %A Actual: %A Error = %A" putCpu.[i] putParallel.[i] (abs putCpu.[i] - putParallel.[i])
            if isSuccess2 && abs (callCpu.[i] - callParallel.[i]) > Eps
                then
                    isSuccess2 <- false
                    printfn "Call. Expected: %A Actual: %A Error = %A" callCpu.[i] callParallel.[i] (abs callCpu.[i] - callParallel.[i])
                            
        printfn "done."
    
        Timer<string>.Global.Average(".NET") |> printfn "Avg. time, C#: %A"
        Timer<string>.Global.Total("OpenCL") / float iterations |> printfn "Avg. time, OpenCL: %A"
    Main()

do main()