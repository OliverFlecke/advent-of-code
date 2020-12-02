#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Debug/net5.0/AdventOfCode.dll"

open AdventOfCode.Core

let data = input 2020 2

let count c (str: string) =
    str |> Seq.filter (fun x -> x = c) |> Seq.length

let toRange (r: string) =
    match r.Split("-") |> Seq.map int |> Seq.toList with
    | s :: e :: _ -> (s, e)
    | _ -> failwith "Unable to parse range"

let toTuple =
    function
    | [| r; c; s |] -> (toRange r, Seq.head c, s)
    | _ -> failwith "unexpected number of elements in line"

let parse (d: string) =
    d.Trim().Split("\n")
    |> Seq.map ((fun (str: string) -> str.Split " ") >> toTuple)

// Level 1
let validate ((s, e), c, str) =
    count c str |> (fun n -> s <= n && n <= e)

let a =
    parse data |> Seq.filter validate |> Seq.length

submit 2020 2 Level.One a

// Level 2
let validate2 (((i, j): int * int), c, (str: string)) = (str.[i - 1] = c) <> (str.[j - 1] = c)

let b =
    parse data |> Seq.filter validate2 |> Seq.length

submit 2020 2 Level.Two b
