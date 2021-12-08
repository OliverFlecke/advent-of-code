#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open System
open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2016 8

let parse = splitLines >> Seq.ofArray
let width, height = 50, 6

let printScreen screen =
    screen
    |> (Array2D.map (fun b -> if b then "#" else " ")
        >> transpose
        >> Seq.cast<string>
        >> take (Array2D.length1 screen)
        >> Seq.map String.Concat
        >> String.concat "\n"
        >> printfn "%s")

let solve width height lines =
    let initial =
        Array2D.init width height (fun _ _ -> false)

    let execute screen str =
        match str with
        | ReMatch "rect (\d+)x(\d+)" [ a; b ] ->
            screen
            |> Array2D.mapi (fun x y v -> (x < int a && y < int b) || v)

        | ReMatch "rotate row y=(\d+) by (\d+)" [ row; amount ] ->
            screen
            |> Array2D.mapi (fun x y v ->
                if int row <> y then
                    v
                else
                    screen.[modulo width (x - int amount), y])
        | ReMatch "rotate column x=(\d+) by (\d+)" [ col; amount ] ->
            screen
            |> Array2D.mapi (fun x y v ->
                if int col <> x then
                    v
                else
                    screen.[x, modulo height (y - int amount)])
        | _ -> failwith $"Unhandled instruction: {str}"

    Seq.fold execute initial lines

let solver =
    parse
    >> solve width height
    >> Seq.cast<bool>
    >> Seq.filter id
    >> Seq.length

// "rect 3x2\nrotate column x=1 by 1\nrotate row y=0 by 4\nrotate column x=1 by 1"
// |> (parse >> solve 7 3)
// |> printScreen 7

let a = solver data
submit 2016 8 Level.One a

// Part B
let solver' = parse >> solve width height >> printScreen

pSuccess "Solution for part B:"
solver' data
