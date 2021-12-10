#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Types
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

let closing (c: char) =
    c = '}' || c = ')' || c = ']' || c = '>'

let opposite c =
    match c with
    | '{' -> '}'
    | '(' -> ')'
    | '[' -> ']'
    | '<' -> '>'
    | _ -> failwith $"Unsupported character '{c}'"

let analyze str =
    let rec analyze' parsed rest =
        match parsed, rest with
        | _, [] -> Left <| Seq.map opposite parsed
        | p :: _, n :: _ when closing n && opposite p <> n -> Right n
        | _ :: parsed', n :: rest' when closing n -> analyze' parsed' rest'
        | _, n :: rest' -> analyze' (n :: parsed) rest'

    analyze' [] str

let solve =
    List.map (List.ofSeq >> analyze)
    >> Seq.filter Either.isRight
    >> Seq.map Either.rightValue
    >> Seq.sumBy points

let solver = parse >> solve

testSolution Level.One 26397 <| solver testData
submit 2021 10 Level.One <| solver data

// Part B
let points' c =
    match c with
    | ')' -> 1
    | ']' -> 2
    | '}' -> 3
    | '>' -> 4
    | _ -> failwith "Unsupported point"

let solve' input =
    let values =
        input
        |> (List.map (List.ofSeq >> analyze)
            >> Seq.filter Either.isLeft
            >> Seq.map (
                Either.leftValue
                >> Seq.map (points' >> int64)
                >> Seq.reduce (fun acc v -> acc * 5L + v)
            )
            >> List.ofSeq
            >> List.sort)

    values.[values.Length / 2]

let solver' = parse >> solve'

testSolution Level.Two 288957L <| solver' testData
submit 2021 10 Level.Two <| solver' data
