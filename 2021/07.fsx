#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2021 7
let testData = testInput 2021 7

let parse = splitComma >> Seq.map int

let solve cost (crabs: seq<int>) =
    let fuel crabs pos =
        crabs
        |> Seq.map ((-) pos >> abs >> cost)
        |> Seq.sum

    [ Seq.min crabs .. Seq.max crabs ]
    |> Seq.map (fuel crabs)
    |> Seq.min

let solver = parse >> solve id

testSolution Level.One 37 <| solver testData
submit 2021 7 Level.One <| solver data

// Part B
let solver' = parse >> solve sumOfNumbers

testSolution Level.Two 168 <| solver' testData
submit 2021 7 Level.Two <| solver' data
