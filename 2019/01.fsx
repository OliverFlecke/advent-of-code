#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Debug/net5.0/AdventOfCode.dll"

open AdventOfCode.Utils

let input = getInput 2019 1

let fuel m = (int) (m / 3) - 2

let rec fuelRec m =
    let f = fuel m
    if f <= 0 then 0 else f + fuelRec f

let numbers = ints input
let a = numbers |> Seq.sumBy fuel
submit 2019 1 1 a

let b = numbers |> Seq.sumBy fuelRec
submit 2019 1 2 b
