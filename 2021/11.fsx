#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Types
open AdventOfCode.Utils

let data = input 2021 11
let testData = testInput 2021 11

let parse =
    splitLines
    >> array2D
    >> Array2D.map (int >> (flip (-) <| int '0'))

let neighbors map p =
    directions
    |> Seq.map (fun (x, y) -> x + fst p, y + snd p)
    |> Seq.filter (fun (x, y) ->
        0 <= x
        && x < Array2D.length1 map
        && 0 <= y
        && y < Array2D.length2 map)

let solve amount input =
    let step1 = Array2D.map ((+) 1)

    let step2 grid =
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

        run grid

    let numberOfFlashes =
        Seq.cast<int> >> Seq.filter ((=) 0) >> Seq.length

    let iter = step1 >> step2

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