namespace AdventOfCode.Solutions

open System.Text.RegularExpressions
open AdventOfCode
open AdventOfCode.Utils

[<AutoOpen>]
module private Day5 =
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

    let lineSequence ((x0, y0), (x1, y1)) =
        let dir s e =
            if s = e then 0
            elif s < e then 1
            else -1

        let xDir = dir x0 x1

        let yDir = dir y0 y1

        seq {
            for i = 0 to max (abs (x0 - x1)) (abs (y0 - y1)) do
                yield (x0 + i * xDir, y0 + i * yDir)
        }

    let markBoard board line =
        for (x, y) in lineSequence line do
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

type Year2021Day5() =
    interface ISolution with
        member _.year = 2021
        member _.day = 5

        member _.testA = seq [ (Int 5, None) ]
        member _.testB = seq [ (Int 12, None) ]

        member _.solveA input =
            Int
            <| (parse >> filterHorizontalAndVertical >> solve) input

        member _.solveB input = Int <| (parse >> solve) input
