#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2021 2
let testData = "forward 5
down 5
forward 8
up 3
down 8
forward 2"

type Command =
    | Forward of int
    | Down of int
    | Up of int

let parseCommand command =
    match command with
    | ReMatch "forward (?<amount>\d+)" [ _; amount ] -> Forward <| int amount.Value
    | ReMatch "down (?<amount>\d+)" [ _; amount ] -> Down <| int amount.Value
    | ReMatch "up (?<amount>\d+)" [ _; amount ] -> Up <| int amount.Value
    | _ -> failwith $"Unable to understand command '{command}'"

let parse = splitLines >> List.ofSeq >> List.map parseCommand

let solve commands =
    let rec helper (h, d) commands =
        match commands with
        | [] -> (h, d)
        | Forward x :: rest -> helper (h + x, d) rest
        | Up x :: rest -> helper (h, d - x) rest
        | Down x :: rest -> helper (h, d + x) rest

    let (h, d) = helper (0, 0) commands
    h * d

let solver = parse >> solve

// Test and submit A
testSolution Level.One 150 <| solver testData

let a = solver data
submit 2021 2 Level.One a


// Part B
let solve' commands =
    let rec helper (horizontal, depth, aim) commands =
        match commands with
        | [] -> (horizontal, depth, aim)
        | Forward x :: rest -> helper (horizontal + x, depth + aim * x, aim) rest
        | Up x :: rest -> helper (horizontal, depth, aim - x) rest
        | Down x :: rest -> helper (horizontal, depth, aim + x) rest

    let (h, d, _) = helper (0, 0, 0) commands
    h * d

let solver' = parse >> solve'

// Test and submit B
testSolution Level.Two 900 <| solver' testData

let b = solver' data
submit 2021 2 Level.Two b