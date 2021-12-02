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

let solve transform initial commands =
    let rec helper state commands =
        match commands with
        | [] -> state
        | command :: rest -> helper (transform state command) rest

    helper initial commands

let transform (h, d) command =
    match command with
    | Forward x -> (h + x, d)
    | Up x -> (h, d - x)
    | Down x -> (h, d + x)

let solver = parse >> solve transform (0, 0) >> (fun (h, d) -> h * d)

// Test and submit A
testSolution Level.One 150 <| solver testData

let a = solver data
submit 2021 2 Level.One a


// Part B
let transform' (h, d, a) command =
    match command with
    | Forward x -> (h + x, d + a * x, a)
    | Up x -> (h, d, a - x)
    | Down x -> (h, d, a + x)

let solver' = parse >> solve transform' (0, 0, 0) >> (fun (h, d, _) -> h * d)

// Test and submit B
testSolution Level.Two 900 <| solver' testData

let b = solver' data
submit 2021 2 Level.Two b