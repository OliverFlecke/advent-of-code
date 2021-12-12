namespace AdventOfCode.Solutions

open AdventOfCode
open AdventOfCode.Utils

type Year2021Day1() =
    member x.parse = ints

    member x.solve numbers =
        let counter (prev, count) x =
            if x > prev then
                (x, count + 1)
            else
                (x, count)

        numbers
        |> Seq.tail
        |> Seq.fold counter (Seq.head numbers, 0)
        |> snd

    interface ISolution with
        member _.year = 2021
        member _.day = 1

        member self.solveA input = input |> (self.parse >> self.solve >> string)

        member self.solveB input =
            let transform (numbers: seq<int>) =
                numbers |> Seq.windowed 3 |> Seq.map Array.sum

            input |> (self.parse >> transform >> self.solve >> string)
