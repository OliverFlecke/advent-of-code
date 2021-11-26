#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net5.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2016 1
let testData = "R5, L5, R5, R3"

type Direction = North | East | South | West
type Step = Left of int | Right of int

let toDir (step: string) =
    let number = int step[1..]
    if step[0] = 'L'
        then Left(number)
        else Right(number)

let parse (input : string) =
    input.Split(", ")
    |> Seq.map toDir

let rotate dir step =
    match step with
    | Left(_) ->
        match dir with
        | North -> West
        | West -> South
        | South -> East
        | East -> North
    | Right(_) ->
        match dir with
        | North -> East
        | East -> South
        | South -> West
        | West -> North

let getStepAmount step =
    match step with
    | Left(x) -> x
    | Right(x) -> x

let move dir amount pos =
    match dir with
    | North -> (fst pos + amount, snd pos)
    | South -> (fst pos - amount, snd pos)
    | East -> (fst pos, snd pos + amount)
    | West -> (fst pos, snd pos - amount)

let solve steps =
    let rec helper dir pos steps =
        match steps with
        | [] -> pos
        | step :: tail ->
            let newDir = rotate dir <| step
            let amount = getStepAmount step
            helper newDir (move newDir amount pos) tail

    let pos = helper North (0, 0) steps
    fst pos + snd pos

let solver = parse >> List.ofSeq >> solve

let a = solver data
submit 2016 1 Level.One a

// Part B
type Visited = Set<int * int>

let solveB steps =
    let rec checkStep dir pos step (visited: Visited) =
        if step = 0
            then (None, visited)
            else
                let newPos = move dir 1 pos
                if visited.Contains newPos
                    then (Some newPos, visited)
                    else checkStep dir newPos (step - 1) (visited.Add newPos)
    let rec helper dir pos steps (visited: Visited) =
        match steps with
        | [] -> pos
        | step :: tail ->
            let newDir = rotate dir <| step
            let amount = getStepAmount step

            let (repeated, newVisited) = checkStep newDir pos amount visited
            match repeated with
                | Some(pos) -> pos
                | None ->
                    let newPos = move newDir amount pos
                    helper newDir newPos tail newVisited

    let pos = helper North (0, 0) steps Set.empty
    fst pos + snd pos

let solverB = parse >> List.ofSeq >> solveB

// Test data
"R8, R4, R4, R8" |> solverB |> printfn "Part B: Should be 4, is %d"

let b = solverB data
submit 2016 1 Level.Two b
