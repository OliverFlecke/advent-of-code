open System.Collections.Generic
#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Debug/net5.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2019 1

let fuel m = (int) (m / 3) - 2

let rec fuelRec m =
    let f = fuel m
    if f <= 0 then 0 else f + fuelRec f

let numbers = ints data
let a = numbers |> Seq.sumBy fuel
submit 2019 1 Level.One a

let b = numbers |> Seq.sumBy fuelRec
submit 2019 1 Level.Two b
