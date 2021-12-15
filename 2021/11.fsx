#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2021 11
let testData = testInput 2021 11

let parse =
    splitLines
    >> array2D
    >> Array2D.map (int >> (flip (-) <| int '0'))

let iter grid =
    let rec run grid =
        let next =
            grid
            |> Array2D.mapi (fun x y v ->
                if v = 0 || v > 9 then
                    0
                else
                    let neighboringFlashes =
                        neighbors grid (x, y)
                        |> Seq.map (uncurry <| Array2D.get grid)
                        |> count 10

                    min 10 <| v + neighboringFlashes)

        if next = grid then next else run next

    grid |> Array2D.map ((+) 1) |> run

let numberOfFlashes =
    Seq.cast<int> >> Seq.filter ((=) 0) >> Seq.length

let solve amount input =
    Seq.fold
        (fun (flashes, grid) _ ->
            let next = iter grid

            flashes + numberOfFlashes next, next)
        (0, input)
        [ 0 .. amount - 1 ]
    |> fst

let solver = parse >> solve 100

testSolution Level.One 1656 <| solver testData
submit 2021 11 Level.One <| solver data

// Part B
let solve' grid =
    Seq.scan (fun grid _ -> iter grid) grid (Seq.initInfinite id)
    |> Seq.takeWhile (Seq.cast<int> >> Seq.exists ((<>) 0))
    |> Seq.length

let solver' = parse >> solve'

testSolution Level.Two 195 <| solver' testData
submit 2021 11 Level.Two <| solver' data