namespace AdventOfCode.Solutions

open System
open AdventOfCode
open AdventOfCode.Utils

[<AutoOpen>]
module private Day14 =
    let parse (str: string) =
        let splits = str.Split("\n\n")
        let start = splits.[0]

        let mapping =
            splits.[1]
            |> splitLines
            |> Seq.map (fun line -> line.Split(" -> "))
            |> Seq.fold (fun map line -> Map.add line.[0] line.[1].[0] map) Map.empty

        start, mapping

    let iter mapping (str: string) =
        str
        |> Seq.windowed 2
        |> Seq.map (fun pair ->
            Map.tryFind (String.Concat pair) mapping
            |> Option.map (fun c -> [| pair.[0]; c |])
            |> Option.defaultValue pair.[..1])
        |> Seq.collect id
        |> String.Concat
        |> flip (+) str.[str.Length - 1..]


type Year2021Day14() =
    interface ISolution with
        member _.year = 2021
        member _.day = 14

        member _.testA = seq [ (Int 1588, None) ]
        member _.testB = seq []

        member self.solveA input =
            let start, mapping = parse input

            let finished =
                Seq.fold (fun str _ -> iter mapping str) start [ 0 .. 9 ]

            let frequency =
                finished
                |> Seq.groupBy id
                |> Seq.map (fun (c, ls) -> Seq.length ls)
                |> Seq.sort

            Int <| Seq.last frequency - Seq.head frequency

        member self.solveB input = Int 0
