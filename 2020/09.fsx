#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2020 9
let testData = testInput 2020 9

let parse (str: string) =
    str.Trim() |> splitLines |> Seq.map int64

let isValid (preamble: int) (numbers: seq<int64>) =
    let pairs =
        Seq.take preamble numbers
        |> List.ofSeq
        |> combinations 2
        |> Seq.map (Seq.sum)
        |> Seq.distinct

    let next = Seq.item preamble numbers

    Seq.contains next pairs

let solve preamble =
    parse
    >> Seq.windowed (preamble + 1)
    >> Seq.skipWhile (isValid preamble)
    >> Seq.head
    >> Seq.last

solve 5 testData |> print

let a = solve 25 data
submit 2020 9 Level.One <| a

// Level 2
let calculateWeakness (lst: seq<int64>) = Seq.min lst + Seq.max lst

let weakness goal numbers =
    let mutable sum = 0L
    let mutable start = -1
    let mutable i = 0

    while sum <> goal do
        start <- start + 1
        sum <- 0L
        i <- 0

        let ns = Array.skip start numbers
        while sum < goal do
            sum <- sum + ns.[i]
            i <- i + 1

    let lst = numbers |> Seq.skip start |> Seq.take i
    calculateWeakness lst


let solve2 (goal: int64) =
    parse >> Array.ofSeq >> weakness (int64 goal)

solve2 127L testData |> print
submit 2020 9 Level.Two <| solve2 a data

// Functional solution to part 2
let rec weakness2 goal acc lst tail =
    match tail with
    | [] -> if acc = goal then Some(calculateWeakness lst) else None
    | (x :: xs) ->
        match acc with
        | _ when acc > goal -> None
        | _ when acc = goal -> Some(calculateWeakness lst)
        | _ -> weakness2 goal (acc + x) (x :: lst) xs

let solve2' (goal: int64) =
    parse
    >> List.ofSeq
    >> tails
    >> mapOption (weakness2 goal 0L [])
    >> Seq.head
    >> Option.get

submit 2020 9 Level.Two <| solve2' a data
