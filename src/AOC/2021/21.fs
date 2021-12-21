namespace AdventOfCode.Solutions

open AdventOfCode
open AdventOfCode.Utils
open System.Collections.Generic

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

[<AutoOpen>]
module private Day21B =
    let permutations =
        seq {
            for i in 1 .. 3 do
                for j in 1 .. 3 do
                    for k in 1 .. 3 do
                        yield i, j, k
        }

    let solve' (p1, p2) =
        let cache =
            new Dictionary<int * int * int * int, uint64 * uint64>()

        let rec wins (s1, s2) (p1, p2) =
            if s1 >= 21 then
                1UL, 0UL
            elif s2 >= 21 then
                0UL, 1UL
            else
                let key = (p1, p2, s1, s2)

                let hit, value = cache.TryGetValue key

                if hit then
                    value
                else
                    let mutable a, b = 0UL, 0UL

                    for (d1, d2, d3) in permutations |> List.ofSeq do
                        let p1' = (p1 + d1 + d2 + d3) % 10
                        let s1' = s1 + p1' + 1

                        let x1, y1 = wins (s2, s1') (p2, p1')
                        do a <- a + y1
                        do b <- b + x1

                    do cache.[key] <- (a, b)
                    a, b

        wins (0, 0) (p1 - 1, p2 - 1)
        |> uncurry max
        |> uint64


type Year2021Day21() =
    interface ISolution with
        member _.year = 2021
        member _.day = 21

        member _.testA = seq [ (Int 739785, None) ]
        member _.testB = seq [ (UInt64 444356092776315UL, None) ]

        member _.solveA input = input |> parse |> solve |> Int

        member _.solveB input = input |> parse |> solve' |> UInt64
