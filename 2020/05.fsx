#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2020 5

let row (str: string) =
    let binary =
        str
        |> Seq.take 7
        |> Seq.map (function
            | 'F' -> "0"
            | 'B' -> "1"
            | c -> failwith $"unknow character {c}")
        |> Seq.reduce (+)

    System.Convert.ToInt32(binary, 2)

let column (str: string) =
    let binary =
        str
        |> Seq.skip 7
        |> Seq.map (function
            | 'R' -> "1"
            | 'L' -> "0"
            | c -> failwith $"Unknown character {c}")
        |> Seq.reduce (+)

    System.Convert.ToInt32(binary, 2)

let seat str = (row str, column str)
let seatId (r, c) = 8 * r + c

let parse = splitLines

let solve =
    parse >> Seq.map (seat >> seatId) >> Seq.max

submit 2020 5 Level.One <| solve data

// Level 2
let boardingPasses =
    parse >> Seq.map (seat) >> Set.ofSeq <| data

let ids = boardingPasses |> Seq.map seatId
let min = ids |> Seq.min
let max = ids |> Seq.max

let allSeats =
    seq {
        for r in 0 .. 127 do
            for c in 0 .. 7 -> (r, c)
    }
    |> Seq.filter (fun x ->
        seatId x
        |> fun id -> id > min && id < max)
    |> Set.ofSeq


let solve2 =
    Set.difference allSeats boardingPasses
    |> Seq.head
    |> seatId

submit 2020 5 Level.Two <| solve2
