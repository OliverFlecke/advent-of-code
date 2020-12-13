#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Debug/net5.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2020 10
let testData2 = testInput 2020 10

let testData = """16
10
15
5
1
11
7
19
6
12
4"""

let parse =
    ints
    >> (fun ls ->
        seq {
            yield 0
            yield! ls
            yield Seq.max ls + 3
        })

let diff =
    function
    | [| a; b |] -> b - a
    | _ -> failwith "Wrong window size"

let answer ls = count 1 ls * count 3 ls

let diffs = Seq.windowed 2 >> Seq.map diff

let solve = parse >> Seq.sort >> diffs >> answer

solve testData |> print

submit 2020 10 Level.One <| solve data
// Level 2
let rec arrangements =
    function
    | 1 :: 1 :: 1 :: 1 :: ls -> arrangements ls * 7L
    | 1 :: 1 :: 1 :: ls -> arrangements ls * 4L
    | 1 :: 1 :: ls -> arrangements ls * 2L
    | _ :: ls -> arrangements ls
    | [] -> 1L

let solve2 =
    parse
    >> Seq.sort
    >> diffs
    >> Seq.toList
    >> arrangements

solve2 testData |> print
solve2 testData2 |> print

submit 2020 10 Level.Two <| solve2 data
