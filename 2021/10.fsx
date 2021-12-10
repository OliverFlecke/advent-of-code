#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2021 10
let testData = testInput 2021 10

let parse = splitLines >> List.ofArray

let points c =
    match c with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> failwith $"Unable to get point for '{c}'"

let solve (input: string list) =
    let closing (c: char) =
        c = '}' || c = ')' || c = ']' || c = '>'

    let opposite c =
        match c with
        | '{' -> '}'
        | '(' -> ')'
        | '[' -> ']'
        | '<' -> '>'
        | _ -> failwith $"Unsupported character '{c}'"

    let rec helper parsed rest =
        match parsed, rest with
        | _, [] -> None
        | p :: _, n :: _ when closing n && opposite p <> n -> Some n
        | _ :: parsed', n :: rest' when closing n -> helper parsed' rest'
        | _, n :: rest' -> helper (n :: parsed) rest'

    input
    |> List.map (List.ofSeq >> helper [])
    |> filterOption
    |> Seq.sumBy points

let solver = parse >> solve

testSolution Level.One 26397 <| solver testData
submit 2021 10 Level.One <| solver data
