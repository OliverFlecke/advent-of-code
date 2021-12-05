#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open System.Text.RegularExpressions
open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2021 5

let testData =
    "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"

type Line = (int * int) * (int * int)

let parseLine (str: string) =
    let m =
        Regex.Match(str, "(?<x0>\d+),(?<y0>\d+) -> (?<x1>\d+),(?<y1>\d+)", RegexOptions.Compiled)

    if m.Success then
        m.Groups.Values
        |> Seq.skip 1
        |> List.ofSeq
        |> List.map (fun x -> int x.Value)
        |> (fun ns -> ((ns.[0], ns.[1]), (ns.[2], ns.[3])))
    else
        failwith $"Unable to parse {str}"

let parse =
    splitLines >> Array.map parseLine >> List.ofArray

let filterHorizontalAndVertical lines =
    List.filter (fun ((x0, y0), (x1, y1)) -> x0 = x1 || y0 = y1) lines

let intersects (x, y) ((x0, y0), (x1, y1)) =
    x0 <= x && x <= x1 && y0 <= y && y <= y1

let markBoard board ((x0, y0), (x1, y1)) =
    for x = min x0 x1 to max x0 x1 do
        for y = min y0 y1 to max y0 y1 do
            Array2D.set board x y <| board.[x, y] + 1

    board

let solve lines =
    let xMax =
        lines
        |> List.map (fun ((x0, _), (x1, _)) -> max x0 x1)
        |> List.max
        |> (+) 1

    let yMax =
        lines
        |> List.map (fun ((_, y0), (_, y1)) -> max y0 y1)
        |> List.max
        |> (+) 1

    let board = Array2D.init xMax yMax (fun _ _ -> 0)

    lines
    |> List.fold markBoard board
    |> Seq.cast<int>
    |> Seq.filter ((<=) 2)
    |> Seq.length

let solver =
    parse >> filterHorizontalAndVertical >> solve

testSolution Level.One 5 <| solver testData

submit 2021 5 Level.One <| solver data
