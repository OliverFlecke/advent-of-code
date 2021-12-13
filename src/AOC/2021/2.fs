namespace AdventOfCode.Solutions

open AdventOfCode
open AdventOfCode.Utils

[<AutoOpen>]
module private Day2 =
    type Command =
        | Forward of int
        | Down of int
        | Up of int

    let parseCommand command =
        match command with
        | ReMatch "forward (?<amount>\d+)" [ amount ] -> Forward <| int amount
        | ReMatch "down (?<amount>\d+)" [ amount ] -> Down <| int amount
        | ReMatch "up (?<amount>\d+)" [ amount ] -> Up <| int amount
        | _ -> failwith $"Unable to understand command '{command}'"

    let parse =
        splitLines >> List.ofSeq >> List.map parseCommand

    let transform (h, d) command =
        match command with
        | Forward x -> (h + x, d)
        | Up x -> (h, d - x)
        | Down x -> (h, d + x)

type Year2021Day2() =
    interface ISolution with
        member _.year = 2021
        member _.day = 2

        member _.testA = seq [ (Int 150, None) ]
        member _.testB = seq [ (Int 900, None) ]

        member _.solveA input =
            input
            |> (parse
                >> Seq.fold transform (0, 0)
                >> (fun (h, d) -> h * d))
            |> Int

        member _.solveB input =
            let transform' (h, d, a) command =
                match command with
                | Forward x -> (h + x, d + a * x, a)
                | Up x -> (h, d, a - x)
                | Down x -> (h, d, a + x)

            input
            |> (parse
                >> Seq.fold transform' (0, 0, 0)
                >> (fun (h, d, _) -> h * d))
            |> Int
