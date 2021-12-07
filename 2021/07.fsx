#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2021 7
let testData = testInput 2021 7

let parse = splitComma >> Seq.map int

let fuel crabs pos =
    crabs |> Seq.map ((-) pos >> abs) |> Seq.sum

let solve fuel (crabs: seq<int>) =

    [ Seq.min crabs .. Seq.max crabs ]
    |> Seq.map (fuel crabs)
    |> Seq.min

let solver = parse >> solve fuel

testSolution Level.One 37 <| solver testData
submit 2021 7 Level.One <| solver data

// Part B

let fuel' crabs pos =
    crabs
    |> Seq.map ((-) pos >> abs >> (fun x -> [ 1 .. x ]) >> Seq.sum)
    |> Seq.sum

let solver' = parse >> solve fuel'

testSolution Level.Two 168 <| solver' testData
submit 2021 7 Level.Two <| solver' data
