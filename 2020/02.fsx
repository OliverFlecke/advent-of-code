#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Debug/net5.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2020 2

let toRange (r: string) =
    match r.Split("-") |> Seq.map int |> Seq.toList with
    | s :: e :: _ -> (s, e)
    | _ -> failwith "Unable to parse range"

let toTuple =
    function
    | [| r; c; s |] -> (toRange r, Seq.head c, s)
    | _ -> failwith "unexpected number of elements in line"

let parse =
    splitLines >> Seq.map (splitSpaces >> toTuple)

// Level 1
let validate ((s, e), c, str) =
    count c str |> (fun n -> s <= n && n <= e)

let solve =
    parse >> Seq.filter validate >> Seq.length

submit 2020 2 Level.One (solve data)

// Level 2
let validate2 ((i, j), c, (str: string)) = (str.[i - 1] = c) <> (str.[j - 1] = c)

let solve2 =
    parse >> Seq.filter validate2 >> Seq.length

submit 2020 2 Level.Two (solve2 data)
