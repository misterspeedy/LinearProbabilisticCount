#r @"C:\Users\Kit\documents\visual studio 2013\Projects\LinearProbabilisticCount\LinearProbabilisticCount\bin\Debug\LinearProbabilisticCount.dll"

open BigDataCounting
open BigDataCounting.LinearProbabilistic

let r = new System.Random()

let totalCount = 10000000
let cardinality = 1000
let mapSize = 10000<bit>

let big = Array.init totalCount (fun _ -> r.Next(cardinality-1))

#time
printfn "Seq.distinct %i" (big |> Seq.distinct |> Seq.length)
printfn "Count %i" (big |> Count mapSize)
printfn "PCount %i" (big |> PCount mapSize 8)
#time
