namespace AdventOfCode.Solutions

open System
open AdventOfCode
open AdventOfCode.Utils
open FSharp.Collections

[<AutoOpen>]
module private Day8 =
    let is147or8 str =
        match Seq.length str with
        | 2
        | 4
        | 3
        | 7 -> true
        | _ -> false

    let solve =
        splitLines
        >> Array.map (fun str ->
            str.Split("|").[1].Trim()
            |> splitSpaces
            |> Seq.filter is147or8)
        >> Seq.collect id
        >> Seq.length

    let parse' =
        splitLines
        >> Array.map (fun str ->
            let a = str.Split("|")
            (a.[0].Trim() |> splitSpaces, a.[1].Trim() |> splitSpaces))

    let contains (str: string) (c: char) = str.Contains c

    let findDigits signal (digits: seq<string>) =
        let digitFromLength len map signal =
            signal
            |> Seq.filter (Seq.length >> (=) len)
            |> Seq.head

        let find3 (map: Map<int, string>) =
            Seq.find (fun s ->
                Seq.length s = 5
                && Seq.forall (contains s) map.[1])

        let find9 (map: Map<int, string>) signal =
            map.[3] + map.[4] |> Seq.distinct |> String.Concat

        let find0 (map: Map<int, string>) =
            Seq.find (fun s ->
                Seq.length s = 6
                && Seq.forall (contains s) map.[1]
                && not <| Seq.forall (contains s) map.[9])

        let find6 (map: Map<int, string>) =
            Seq.find (fun s ->
                Seq.length s = 6
                && not <| Seq.forall (contains s) map.[0]
                && not <| Seq.forall (contains s) map.[9])

        let find5 (map: Map<int, string>) =
            Seq.find (fun s ->
                Seq.length s = 5
                && Seq.forall (contains map.[6]) s)

        let find2 (map: Map<int, string>) =
            Seq.find (fun s -> Seq.length s = 5 && s <> map.[3] && s <> map.[5])

        let map =
            Seq.fold (fun (map: Map<int, string>) (d, f) -> map.Change(d, (fun _ -> Some <| f map signal))) Map.empty
            <| seq [
                (1, digitFromLength 2)
                (4, digitFromLength 4)
                (7, digitFromLength 3)
                (8, digitFromLength 7)
                (3, find3)
                (9, find9)
                (0, find0)
                (6, find6)
                (5, find5)
                (2, find2) ]

        let lookup =
            map
            |> Map.toSeq
            |> Seq.map (fun (k, v) -> (v |> Seq.sort |> String.Concat, k))
            |> Map.ofSeq

        digits
        |> Seq.map (Seq.sort >> String.Concat)
        |> Seq.map (fun s -> lookup.[s])
        |> Seq.reduce (fun acc d -> acc * 10 + d)

    let solve' = Array.sumBy (uncurry findDigits)

type Year2021Day8() =
    interface ISolution with
        member _.year = 2021
        member _.day = 8

        member _.testA = seq [ (Int 26, None) ]
        member _.testB = seq [ (Int 61229, None) ]

        member self.solveA input = Int <| solve input

        member self.solveB input = Int <| (parse' >> solve') input
