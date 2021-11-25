#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net5.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2020 6
let testData = testInput 2020 6

let parse (str: string) =
    str.Split("\n\n")
    |> Seq.map (fun s -> s.Replace("\n", ""))

let count (input: seq<'b>) = input |> (Seq.distinct >> Seq.length)

let solve = parse >> Seq.sumBy count

printfn "Answer for test data %d"
<| solve testData

submit 2020 6 Level.One <| solve data

// Level 2
let parse2 (str: string) =
    str.Split("\n\n")
    |> Seq.map (splitLines >> Seq.map Set.ofSeq)

let count2 (input: seq<Set<char>>) =
    input |> (Set.intersectMany >> Seq.length)

let solve2 = parse2 >> Seq.sumBy count2

printfn "Answer for test data part 2: %d"
<| solve2 testData

submit 2020 6 Level.Two <| solve2 data
