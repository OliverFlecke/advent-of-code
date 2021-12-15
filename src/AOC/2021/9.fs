namespace AdventOfCode.Solutions

open AdventOfCode
open AdventOfCode.Utils

[<AutoOpen>]
module private Day9 =
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

    let solve' map =
        let rec findBasin (map: int [,]) basin visited frontier =
            match frontier with
            | [] -> basin
            | (x, y) :: frontier' when map.[x, y] = 9 -> findBasin map basin (Set.add (x, y) visited) frontier'
            | point :: frontier' ->
                findBasin
                    map
                    (point :: basin)
                    (Set.add point visited)
                    (frontier'
                     @ (neighbors map point
                        |> Seq.filter (flip Set.contains visited >> not)
                        |> Seq.filter (flip List.contains frontier' >> not)
                        |> List.ofSeq))

        findLowPoints map
        |> Seq.map (fun point -> findBasin map [] Set.empty [ point ])
        |> Seq.map (Seq.length)
        |> Seq.sortDescending
        |> Seq.take 3
        |> Seq.reduce (*)

    let solver' = parse >> solve'

type Year2021Day9() =
    interface ISolution with
        member _.year = 2021
        member _.day = 9

        member _.testA = seq [ (Int 15, None) ]
        member _.testB = seq [ (Int 1134, None) ]

        member _.solveA input = Int <| solver input

        member _.solveB input = Int <| solver' input
