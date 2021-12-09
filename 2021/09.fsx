#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2021 9
let testData = testInput 2021 9

let parse =
    splitLines
    >> Array.map (Seq.map (string >> int))
    >> array2D

let dirs = seq [ (-1, 0); (0, -1); (1, 0); (0, 1) ]

let isInside map (x, y) =
    0 <= x
    && x < Array2D.length1 map
    && 0 <= y
    && y < Array2D.length2 map

let neighbors map point =
    let add (a, b) (c, d) = a + c, b + d

    dirs
    |> Seq.map (add point)
    |> Seq.filter (isInside map)

let findLowPoints map =
    seq {
        for x in 0 .. Array2D.length1 map - 1 do
            for y in 0 .. Array2D.length2 map - 1 do
                if Seq.forall (fun (a, b) -> map.[a, b] > map.[x, y])
                   <| neighbors map (x, y) then
                    yield (x, y)
    }

let solve map =
    findLowPoints map
    |> Seq.sumBy (uncurry <| Array2D.get map >> (+) 1)

let solver = parse >> solve

testSolution Level.One 15 <| solver testData
submit 2021 9 Level.One <| solver data
