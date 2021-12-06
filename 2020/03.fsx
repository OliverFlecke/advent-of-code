#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2020 3

let parse = splitLines >> Array.map Seq.toArray

type Map = char [] []

let rec coordinates (x, y) (a, b) =
    seq {
        yield (x, y)
        yield! coordinates (x + a, y + b) (a, b)
    }

let traverse pos dir (map: Map) =
    coordinates pos dir
    |> Seq.takeWhile (fun (_, y) -> y < Seq.length map)
    |> Seq.map (fun (x, y) -> map.[y].[x % Seq.length map.[y]])

let countTrees slope =
    traverse (0, 0) slope
    >> Seq.filter ((=) '#')
    >> Seq.length

let solve = parse >> countTrees (3, 1)

testInput 2020 3
|> solve
|> printfn "Trees in test data %d"

submit 2020 3 Level.One <| solve data

// Level 2
let slopes =
    seq {
        (1, 1)
        (3, 1)
        (5, 1)
        (7, 1)
        (1, 2)
    }

let countTreesOnSlopes slopes map =
    slopes
    |> Seq.map (fun s -> countTrees s map |> int64)
    |> Seq.reduce (*)

let solve2 = parse >> countTreesOnSlopes slopes

testInput 2020 3
|> solve2
|> printfn "Solve2 for test data %d"

submit 2020 3 Level.Two <| solve2 data
