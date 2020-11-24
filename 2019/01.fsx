#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Debug/net5.0/AdventOfCode.dll"

open AdventOfCode.Utils

let tokenPath = ".token"
let input = getInput tokenPath 2019 1

let fuel m = (int) (m / 3) - 2

let rec fuel_rec m =
    let f = fuel m
    if f <= 0 then 0 else f + fuel_rec f

let numbers = input.Trim().Split("\n") |> Seq.map int
let a = numbers |> Seq.map fuel |> Seq.sum
printfn $"{a}"

let b = numbers |> Seq.map fuel_rec |> Seq.sum
printfn $"{b}"
