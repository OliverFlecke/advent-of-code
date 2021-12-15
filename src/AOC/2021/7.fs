namespace AdventOfCode.Solutions

open AdventOfCode
open AdventOfCode.Utils

[<AutoOpen>]
module private Day7 =
    let parse = splitComma >> Seq.map int

    let solve cost (crabs: seq<int>) =
        let fuel crabs pos =
            crabs
            |> Seq.map ((-) pos >> abs >> cost)
            |> Seq.sum

        [ Seq.min crabs .. Seq.max crabs ]
        |> Seq.map (fuel crabs)
        |> Seq.min

type Year2021Day7() =
    interface ISolution with
        member _.year = 2021
        member _.day = 7

        member _.testA = seq [ (Int 37, None) ]
        member _.testB = seq [ (Int 168, None) ]

        member _.solveA input = Int <| (parse >> solve id) input

        member _.solveB input =
            Int <| (parse >> solve sumOfNumbers) input
