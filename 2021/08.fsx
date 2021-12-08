#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2021 8
let testData = testInput 2021 8

let is147or8 str =
    match Seq.length str with
    | 2
    | 4
    | 3
    | 7 -> true
    | _ -> false

let solve =
    splitLines
    >> Array.map (fun str ->
        str.Split("|").[1].Trim()
        |> splitSpaces
        |> Seq.filter is147or8)
    >> Seq.collect id
    >> Seq.length

testSolution Level.One 26 <| solve testData
submit 2021 8 Level.One <| solve data
