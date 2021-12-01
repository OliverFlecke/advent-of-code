#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Debug/net5.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2021 1
let testData = "199
200
208
210
200
207
240
269
260
263"

type State = int * int
let parse = ints

let solve numbers =
    let counter (prev, count) x = if x > prev then (x, count + 1) else (x, count)
    Seq.tail numbers
    |> Seq.fold counter (Seq.head numbers, 0)
    |> snd

let solver = parse >> solve

printfn "A. Test data is correct: %A" <| (solver testData = 7)

let a = solver data
submit 2021 1 Level.One a

let transform (numbers: seq<int>) = numbers |> Seq.windowed 3 |> Seq.map Array.sum
let solver' = parse >> transform >> solve

printfn "B. Test data is correct: %A" <| (solver' testData = 5)

let b = solver' data
submit 2021 1 Level.Two b
