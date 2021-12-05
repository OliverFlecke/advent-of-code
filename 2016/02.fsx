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
    | (-1, -1) -> "1"
    | (0, -1) -> "2"
    | (1, -1) -> "3"
    | (-1, 0) -> "4"
    | (0, 0) -> "5"
    | (1, 0) -> "6"
    | (-1, 1) -> "7"
    | (0, 1) -> "8"
    | (1, 1) -> "9"
    | _ -> failwith $"Cannot find position for '{pos}'"

let move (x, y) dir =
    match dir with
    | Up -> (x, max -1 <| y - 1)
    | Down -> (x, min 1 <| y + 1)
    | Right -> (min 1 <| x + 1, y)
    | Left -> (max -1 <| x - 1, y)

let solve initial move mapPosition input =
    let folder (last, state) dirs =
        let result = List.fold move last dirs
        (result, state @ [ result ])

    input
    |> List.fold folder (initial, [])
    |> snd
    |> List.map mapPosition
    |> String.concat ""

let solver = parse >> solve (0, 0) move positionToNumber

testSolution Level.One "1985" <| solver testData
submit 2016 2 Level.One <| solver data

// Part B
let move' (x, y) dir =
    let pos =
        match dir with
        | Up -> (x, y - 1)
        | Down -> (x, y + 1)
        | Right -> (x + 1, y)
        | Left -> (x - 1, y)

    if manhattan pos > 2 then
        (x, y)
    else
        pos

let positionToNumber' pos =
    match pos with
    | (0, -2) -> "1"
    | (-1, -1) -> "2"
    | (0, -1) -> "3"
    | (1, -1) -> "4"
    | (-2, 0) -> "5"
    | (-1, 0) -> "6"
    | (0, 0) -> "7"
    | (1, 0) -> "8"
    | (2, 0) -> "9"
    | (-1, 1) -> "A"
    | (0, 1) -> "B"
    | (1, 1) -> "C"
    | (0, 2) -> "D"
    | _ -> failwith $"Cannot map position '{pos}'"

let solver' = (parse >> solve (-2, 0) move' positionToNumber')

testSolution Level.Two "5DB3" <| solver' testData
submit 2016 2 Level.Two <| solver' data
