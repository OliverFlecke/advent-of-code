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

    let solve mapping start amount =
        let frequency =
            Seq.fold (fun str _ -> iter mapping str) start [ 1 .. amount ]
            |> Seq.groupBy id
            |> Seq.map (fun (c, ls) -> Seq.length ls)
            |> Seq.sort
            |> List.ofSeq

        Seq.last frequency - Seq.head frequency

    let groupByCount =
        Seq.groupBy id
        >> Seq.map (fun (x, xs) -> x, Seq.length xs)

    let convert mapping (pair: string) =
        Map.tryFind pair mapping
        |> Option.map (fun c ->
            [ String.Concat [ pair.[0]; c ]
              String.Concat [ c; pair.[1] ] ])
        |> Option.defaultValue [ pair ]

    let solve' mapping (start: string) amount =
        let pairs =
            start
            |> Seq.windowed 2
            |> Seq.map String.Concat
            |> groupByCount
            |> Seq.map (fun (a, b) -> a, int64 b)

        let iter' mapping (pairs: seq<string * int64>) =
            pairs
            |> Seq.map (fun (pair, count) ->
                convert mapping pair
                |> Seq.map (fun p -> p, count))
            |> Seq.collect id
            |> Seq.groupBy fst
            |> Seq.map (fun (s, ns) -> s, int64 <| Seq.sumBy snd ns)


        let frequency =
            Seq.fold (fun pairs _ -> iter' mapping pairs) pairs [ 1 .. amount ]
            |> Seq.map (fun (pair, amount) -> pair.[0], amount)
            |> Seq.groupBy fst
            |> Seq.map (fun (c, ls) -> c, Seq.sumBy snd ls)
            |> Seq.map (fun (c, count) ->
                if c = start.[start.Length - 1] then
                    c, count + 1L
                else
                    c, count)
            |> Seq.map snd
            |> Seq.sort
            |> List.ofSeq

        List.last frequency - List.head frequency

type Year2021Day14() =
    interface ISolution with
        member _.year = 2021
        member _.day = 14

        member _.testA = seq [ (Int64 1588, None) ]
        member _.testB = seq [ (Int64 2188189693529L, None) ]

        member _.solveA input =
            let start, mapping = parse input

            Int64 <| solve' mapping start 10

        member _.solveB input =
            let start, mapping = parse input
            Int64 <| solve' mapping start 40
