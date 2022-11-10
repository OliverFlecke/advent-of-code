namespace AdventOfCode.Solutions

open AdventOfCode
open AdventOfCode.Utils

[<AutoOpen>]
module private Day25 =
    type SeaCucumber =
        | East
        | South

    type World = SeaCucumber option [,]

    let charToSeaCucumber =
        function
        | '>' -> Some East
        | 'v' -> Some South
        | _ -> None

    let toString =
        function
        | Some East -> '>'
        | Some South -> 'v'
        | None -> '.'

    let parse input =
        input
        |> splitLines
        |> Seq.map (Seq.map charToSeaCucumber)
        |> array2D

    let pretty (map: World) =
        let (width, height) = Array2D.length2 map, Array2D.length1 map

        for y in 0 .. height - 1 do
            for x in 0 .. width - 1 do
                printf "%c" <| toString map.[y, x]

            printf "\n"

    let solve (map: World) =
        let (width, height) = Array2D.length2 map, Array2D.length1 map

        let get (map: World) (x, y) =
            let y, x = modulo height y, modulo width x
            map.[y, x]

        let iteration map =
            let mutable moved = false
            // Move east
            let mutable afterEast = Array2D.create height width None

            for y in 0 .. height - 1 do
                for x in 0 .. width - 1 do
                    match get map (x, y) with
                    | None ->
                        if get map (x - 1, y) = Some East then
                            moved <- true
                            afterEast.[y, x] <- Some East
                    | Some East when get map (x + 1, y) = None -> afterEast.[y, x] <- None
                    | Some v -> afterEast.[y, x] <- Some v

            // Move south
            let mutable afterBoth = Array2D.create height width None

            for y in 0 .. height - 1 do
                for x in 0 .. width - 1 do
                    match get afterEast (x, y) with
                    | None ->
                        if get afterEast (x, y - 1) = Some South then
                            moved <- true
                            afterBoth.[y, x] <- Some South
                    | Some South when get afterEast (x, y + 1) = None -> afterBoth.[y, x] <- None
                    | Some v -> afterBoth.[y, x] <- Some v

            moved, afterBoth

        Seq.initInfinite id
        |> Seq.scan (fun ((_, s), i) _ -> iteration s, i + 1) ((true, map), 1)
        |> Seq.takeWhile (fun ((moved, _), _) -> moved)
        |> Seq.map snd
        |> Seq.last

type Year2021Day25() =
    interface ISolution with
        member _.year = 2021
        member _.day = 25

        member _.testA = seq [ Int 58, None ]

        member _.testB = seq []

        member _.solveA input = input |> parse |> solve |> Int

        member _.solveB input = Int 0
