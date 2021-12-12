#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2021 1
let testData = testInput 2021 1

let parse = ints

let solve numbers =
    let counter (prev, count) x = if x > prev then (x, count + 1) else (x, count)
    Seq.tail numbers
    |> Seq.fold counter (Seq.head numbers, 0)
    |> snd

let solver = parse >> solve

testSolution Level.One 7 <| solver testData

let a = solver data
submit 2021 1 Level.One a

// Part B
let transform (numbers: seq<int>) = numbers |> Seq.windowed 3 |> Seq.map Array.sum
let solver' = parse >> transform >> solve

testSolution Level.Two 5 <| solver' testData

let b = solver' data
submit 2021 1 Level.Two b
