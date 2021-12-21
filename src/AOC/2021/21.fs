namespace AdventOfCode.Solutions

open AdventOfCode
open AdventOfCode.Utils

[<AutoOpen>]
module private Day21 =
    let parse input =
        let positions =
            input
            |> splitLines
            |> Array.map (Seq.last >> string >> int)

        positions.[0], positions.[1]

    let solve positions =
        let rec helper (aScore, bScore) rolls (a, b) =
            if bScore >= 1000 then
                // printfn "Result %i * %i = %i" aScore rolls (aScore * rolls)
                aScore * rolls
            else
                let dices =
                    [ (rolls) % 100 + 1
                      (rolls + 1) % 100 + 1
                      (rolls + 2) % 100 + 1 ]

                let move = Seq.sum dices

                let a' = (a + move - 1) % 10 + 1

                // printfn
                //     "Scores: (%3i, %3i). Rolls: %4i. Pos: %A. Dice: %A. New: %2i"
                //     aScore
                //     bScore
                //     rolls
                //     (a, b)
                //     dices
                //     a'

                helper (bScore, aScore + a') (rolls + 3) (b, a')

        helper (0, 0) 0 positions


type Year2021Day21() =
    interface ISolution with
        member _.year = 2021
        member _.day = 21

        member _.testA = seq [ (Int 739785, None) ]
        member _.testB = seq []

        member _.solveA input = input |> parse |> solve |> Int

        member _.solveB input = Int 0
