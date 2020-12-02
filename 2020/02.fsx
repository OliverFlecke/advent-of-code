#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Debug/net5.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2020 2
// let data = """1-3 a: abcde
// 1-3 b: cdefg
// 2-9 c: ccccccccc"""

let count c (str: string) =
    str |> Seq.filter (fun x -> x = c) |> Seq.length

let toRange (r: string) =
    let [ s; e ] =
        r.Split("-") |> Seq.map int |> Seq.toList

    (s, e)

let countFilter ((s, e): (int * int)) c = s <= c && c <= e

let parse (d: string) =
    d.Trim().Split("\n")
    |> Seq.map (fun s -> s.Split(" "))
    |> Seq.map (fun [| r; c; s |] -> (toRange r, Seq.head c, s))

let a =
    parse data
    |> Seq.filter (fun (r, c, str) -> countFilter r <| count c str)
    |> Seq.length

submit 2020 2 Level.One a

let validate2 ((s, e): int * int) c (str: string) = (str.[s - 1] = c) <> (str.[e - 1] = c)

let b =
    parse data
    |> Seq.filter (fun (r, c, str) -> validate2 r c str)
    |> Seq.length

submit 2020 2 Level.Two b
