#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2015 6

let parsePoint (str: string) =
    match str.Split(",") with
    | [| x; y |] -> (int x, int y)
    | _ -> failwith "Unable to parse point"

let parseLine =
    function
    | ReMatch @"(turn on|toggle|turn off) (\d+,\d+) through (\d+,\d+)" [ cmd; s; e ] ->
        (cmd, parsePoint s, parsePoint e)
    | _ -> failwith "Could not parse"

let parse = splitLines >> Seq.map parseLine

type Command = string * (int * int) * (int * int)

let modifyState cmd =
    match cmd with
    | "turn on" -> fun _ -> true
    | "turn off" -> fun _ -> false
    | "toggle" -> fun b -> not b
    | _ -> failwith $"Unknown command {cmd}"

let update modify (lights: 'a [,]) ((cmd, (x0, y0), (x1, y1)): Command) =
    let m = modify cmd
    for x = min x0 x1 to max x0 x1 do
        for y = min y0 y1 to max y0 y1 do
            lights.[x, y] <- m lights.[x, y]


let follow init modify (inst: seq<Command>) =
    let mutable lights = Array2D.init 1000 1000 (fun _ _ -> init)
    Seq.iter (update modify lights) inst
    lights

let countBool (grid: bool [,]) =
    let mutable c = 0
    Array2D.iter (fun b -> if b then c <- c + 1) grid
    c

let solve =
    parse >> follow false modifyState >> countBool

submit 2015 6 Level.One <| solve data

// Level 2
let modifyBrightness cmd =
    match cmd with
    | "turn on" -> fun x -> x + 1
    | "turn off" -> fun x -> max 0 (x - 1)
    | "toggle" -> fun x -> x + 2
    | _ -> failwith $"Unknown command {cmd}"

let countInt (grid: int [,]) =
    let mutable c = 0
    Array2D.iter (fun x -> c <- c + x) grid
    c

let solve2 =
    parse >> follow 0 modifyBrightness >> countInt

submit 2015 6 Level.Two <| solve2 data
