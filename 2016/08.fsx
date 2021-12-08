#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2016 8

let parse = splitLines >> Seq.ofArray

let printScreen width =
    Array2D.map (fun b -> if b then "#" else " ")
    >> transpose
    >> Seq.cast<string>
    >> take width
    >> Seq.map (String.concat "")
    >> String.concat "\n"
    >> printfn "%s"

let solve width height lines =
    let initial =
        Array2D.init width height (fun _ _ -> false)

    let execute screen str =
        match str with
        | ReMatch "rect (\d+)x(\d+)" [ a; b ] ->
            screen
            |> Array2D.mapi (fun x y v ->
                if x < int a && y < int b then
                    // printfn "Setting %A" (x, y)
                    true
                else
                    v)

        | ReMatch "rotate row y=(\d+) by (\d+)" [ row; amount ] ->
            screen
            |> Array2D.mapi (fun x y v ->
                if int row <> y then
                    v
                else
                    let n = modulo width (x - int amount)
                    // printfn "row %A from: %A dif: %d" (x, y) (n, y) (int amount)
                    screen.[n, y])
        | ReMatch "rotate column x=(\d+) by (\d+)" [ col; amount ] ->
            screen
            |> Array2D.mapi (fun x y v ->
                if int col <> x then
                    v
                else
                    let n = (modulo height (y - int amount))
                    // printfn "col %A from: %A dif: %d" (x, y) (x, n) (int amount)
                    screen.[x, n])
        | _ -> failwith $"Unhandled instruction: {str}"

    let helper screen str =
        let screen' = execute screen str
        printfn "Command: %s" str
        printScreen width screen'
        screen'

    Seq.fold helper initial lines

let solver =
    parse
    >> solve 50 8
    >> Seq.cast<bool>
    >> Seq.filter id
    >> Seq.length

// "rect 3x2\nrotate column x=1 by 1\nrotate row y=0 by 4\nrotate column x=1 by 1"
// |> (parse >> solve 7 3)
// |> printScreen 7

// let a = solver data
// submit 2016 8 Level.One a

// Part B
let solver' = parse >> solve 50 8 >> printScreen 50

solver' data
