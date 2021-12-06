#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2020 12
let testData = testInput 2020 12

type Action =
    | North
    | East
    | South
    | West
    | Left
    | Right
    | Forward

let actionToNumber =
    function
    | North -> 0
    | East -> 1
    | South -> 2
    | West -> 3
    | x -> failwith $"Unable to convert action to number {x}"

let numberToAction =
    function
    | 0 -> North
    | 1 -> East
    | 2 -> South
    | 3 -> West
    | x -> failwith $"unable to convert number {x} to action"

let toAction =
    function
    | "N" -> North
    | "S" -> South
    | "E" -> East
    | "W" -> West
    | "L" -> Left
    | "R" -> Right
    | "F" -> Forward
    | x -> failwith $"No action for {x}"

let parseLine =
    function
    | ReMatch @"(?<action>[NSEWLRF])(?<number>\d+)" [ action; number ] -> (toAction action, int number)
    | x -> failwith $"unable to parse line {x}"

let parse = splitLines >> Seq.map parseLine

let rotate dir degrees =
    actionToNumber dir
    + (degrees / 90)
    |> modulo 4
    |> numberToAction

let moveDir (x, y) =
    function
    | (North, amount) -> (x, y + amount)
    | (South, amount) -> (x, y - amount)
    | (East, amount) -> (x + amount, y)
    | (West, amount) -> (x - amount, y)
    | _ -> failwith "Cannot move in the given direction"

let move (pos, dir) =
    function
    | (Right, degrees) -> (pos, rotate dir degrees)
    | (Left, degrees) -> (pos, rotate dir -degrees)
    | (Forward, amount) -> (moveDir pos (dir, amount), dir)
    | step -> (moveDir pos step, dir)

let solve =
    parse
    >> Seq.fold move ((0, 0), East)
    >> fst
    >> manhattan

solve testData |> print

submit 2020 12 Level.One <| solve data
// Level 2

let rotate2 (x, y) degrees =
    match modulo 4 (degrees / 90) with
    | 0 -> (x, y)
    | 1 -> (y, -x)
    | 2 -> (-x, -y)
    | 3 -> (-y, x)
    | _ -> failwith "unsupported dir"

let move2 (pos, waypoint) =
    function
    | (Right, degrees) -> (pos, rotate2 waypoint degrees)
    | (Left, degrees) -> (pos, rotate2 waypoint -degrees)
    | (Forward, amount) ->
        let (x, y) = pos
        let (xw, yw) = waypoint
        ((x + xw * amount, y + yw * amount), waypoint)
    | step -> (pos, moveDir waypoint step)

let solve2 =
    parse
    >> Seq.fold move2 ((0, 0), (10, 1))
    >> fst
    >> manhattan

solve2 testData |> print

submit 2020 12 Level.Two <| solve2 data
