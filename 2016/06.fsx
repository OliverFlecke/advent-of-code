#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2016 6
let testData = testInput 2016 6

let mostCommon xs =
    xs
    |> Seq.groupBy id
    |> Seq.maxBy (snd >> Seq.length)
    |> fst

let getRow row (matrx: 'a [,]) = matrx.[row..row, *] |> Seq.cast<'a>
let getCol col (matrx: 'a [,]) = matrx.[*, col..col] |> Seq.cast<'a>

let parse = splitLines >> array2D

let solve lines =
    [ 0 .. Array2D.length2 lines - 1 ]
    |> Seq.map (fun c -> getCol c lines)
    |> Seq.map mostCommon
    |> Seq.map string
    |> Seq.reduce (+)

let solver = parse >> solve

testSolution Level.One "easter" <| solver testData
submit 2016 6 Level.One <| solver data
