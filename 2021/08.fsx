#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open System
open AdventOfCode.Core
open AdventOfCode.Utils
open FSharp.Collections

let data = input 2021 8
let testData = testInput 2021 8

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

testSolution Level.One 26 <| solve testData
submit 2021 8 Level.One <| solve data

// Part B
let parse' =
    splitLines
    >> Array.map (fun str ->
        let a = str.Split("|")
        (a.[0].Trim() |> splitSpaces, a.[1].Trim() |> splitSpaces))

let contains (str: string) (c: char) = str.Contains c

let findDigits signal (digits: seq<string>) =
    let digitFrom len =
        signal
        |> Seq.filter (Seq.length >> (=) len)
        |> Seq.head

    let find3 (map: Map<int, string>) =
        Seq.find (fun s ->
            Seq.length s = 5
            && Seq.forall (contains s) map.[1])

    let find9 (map: Map<int, string>) =
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

    let mutable map =
        Map [ (1, digitFrom 2)
              (4, digitFrom 4)
              (7, digitFrom 3)
              (8, digitFrom 7) ]

    let map =
        map.Change(3, (fun _ -> Some <| find3 map signal))

    let map =
        map.Change(9, (fun _ -> Some <| find9 map))

    let map =
        map.Change(0, (fun _ -> Some <| find0 map signal))

    let map =
        map.Change(6, (fun _ -> Some <| find6 map signal))

    let map =
        map.Change(5, (fun _ -> Some <| find5 map signal))

    let map =
        map.Change(2, (fun _ -> Some <| find2 map signal))

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
let solver' = parse' >> solve'

testSolution Level.Two 61229 <| solver' testData
submit 2021 8 Level.Two <| solver' data
