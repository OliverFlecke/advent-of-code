#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Debug/net5.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2015 1

let a = count '(' data - count ')' data

submit 2015 1 Level.One a

let rec track floor inst =
    seq {
        match inst with
        | '(' :: tail ->
            yield floor + 1
            yield! track (floor + 1) tail
        | ')' :: tail ->
            yield floor - 1
            yield! track (floor - 1) tail
        | x -> failwith $"unsupported character '{x}'"
    }

let solve2 (data: string) =
    data
    |> (Seq.toList
        >> track 0
        >> Seq.takeWhile ((<>) -1)
        >> Seq.length
        >> (+) 1)

let b = solve2 data

submit 2015 1 Level.Two (solve2 data)
