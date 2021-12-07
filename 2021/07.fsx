#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2021 7
let testData = testInput 2021 7

let parse = splitComma >> Seq.map int

let solve (positions: seq<int>) =
    let fuel pos =
        positions |> Seq.map ((-) pos >> abs) |> Seq.sum

    [ Seq.min positions .. Seq.max positions ]
    |> Seq.map fuel
    |> Seq.min

let solver = parse >> solve

testSolution Level.One 37 <| solver testData
submit 2021 7 Level.One <| solver data
