#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2016 2
let testData = testInput 2016 2

type Direction =
    | Left
    | Right
    | Up
    | Down

let charToDir c =
    match c with
    | 'U' -> Up
    | 'L' -> Left
    | 'D' -> Down
    | 'R' -> Right
    | _ -> failwith "Unhandled direction"

let parse =
    splitLines
    >> Array.map (Seq.map charToDir >> List.ofSeq)
    >> List.ofArray

let positionToNumber pos =
    match pos with
    | (0, 0) -> 1
    | (1, 0) -> 2
    | (2, 0) -> 3
    | (0, 1) -> 4
    | (1, 1) -> 5
    | (2, 1) -> 6
    | (0, 2) -> 7
    | (1, 2) -> 8
    | (2, 2) -> 9
    | _ -> failwith $"Cannot find position for '{pos}'"

let solve input =
    let rec moveToNumber (x, y) dirs =
        match dirs with
        | [] -> (x, y)
        | d :: dirs' ->
            match d with
            | Up -> moveToNumber (x, max 0 <| y - 1) dirs'
            | Down -> moveToNumber (x, min 2 <| y + 1) dirs'
            | Right -> moveToNumber (min 2 <| x + 1, y) dirs'
            | Left -> moveToNumber (max 0 <| x - 1, y) dirs'

    let folder (last, state) dirs =
        let result = moveToNumber last dirs
        (result, state @ [ result ])

    List.fold folder ((1, 1), []) input
    |> snd
    |> List.map positionToNumber
    |> List.reduce (fun acc c -> acc * 10 + c)

testSolution Level.One 1985
<| (parse >> solve) testData

submit 2016 2 Level.One <| (parse >> solve) data
