#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2016 6
let testData = testInput 2016 6

let parse = splitLines >> array2D

let solve code lines =
    [ 0 .. Array2D.length2 lines - 1 ]
    |> Seq.map ((flip getCol) lines >> code >> string)
    |> Seq.reduce (+)

let solver = parse >> solve mostCommon

testSolution Level.One "easter" <| solver testData
submit 2016 6 Level.One <| solver data

// Part B
let solver' = parse >> solve leastCommon

testSolution Level.Two "advent" <| solver' testData
submit 2016 6 Level.Two <| solver' data
