#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net5.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2020 1
let numbers = ints data

let solve c =
    numbers
    |> Seq.toList
    |> combinations c
    |> Seq.filter (fun ls -> ls |> Seq.take c |> Seq.sum = 2020)
    |> Seq.head
    |> Seq.reduce ((*))

let a = solve 2
submit 2020 1 Level.One a

let b = solve 3
submit 2020 1 Level.Two b
