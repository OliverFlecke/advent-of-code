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

// Parsing
let parseCommand command =
    match command with
    | ReMatch "forward (?<amount>\d+)" [ amount ] -> Forward <| int amount
    | ReMatch "down (?<amount>\d+)" [ amount ] -> Down <| int amount
    | ReMatch "up (?<amount>\d+)" [ amount ] -> Up <| int amount
    | _ -> failwith $"Unable to understand command '{command}'"

let parse = splitLines >> List.ofSeq >> List.map parseCommand

// Part A
let transform (h, d) command =
    match command with
    | Forward x -> (h + x, d)
    | Up x -> (h, d - x)
    | Down x -> (h, d + x)

let solve = parse >> Seq.fold transform (0, 0) >> (fun (h, d) -> h * d)

// Test and submit A
testSolution Level.One 150 <| solve testData

let a = solve data
submit 2021 2 Level.One a

// Part B
let transform' (h, d, a) command =
    match command with
    | Forward x -> (h + x, d + a * x, a)
    | Up x -> (h, d, a - x)
    | Down x -> (h, d, a + x)

let solve' = parse >> Seq.fold transform' (0, 0, 0) >> (fun (h, d, _) -> h * d)

// Test and submit B
testSolution Level.Two 900 <| solve' testData

let b = solve' data
submit 2021 2 Level.Two b
