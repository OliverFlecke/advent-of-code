#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2020 11
let testData = testInput 2020 11

let parse =
    splitLines
    >> Array.ofSeq
    >> fun ss -> Array2D.init (Array.length ss) (Seq.length ss.[0]) (fun x y -> ss.[x].[y])

let print2D (map: char [,]) =
    Array2D.iteri (fun y x v ->
        printf "%c" v
        if Array2D.length1 map = x + 1 then print "") map

let seatExists map (x, y) =
    (0 <= x && x < Array2D.length1 map)
    && (0 <= y && y < Array2D.length2 map)

let adjacents map x y =
    directions
    |> Seq.map (fun (i, j) -> (x + i, y + j))
    |> Seq.filter (seatExists map)

let occupiedAdjacents map x y =
    adjacents map x y
    |> Seq.filter (fun (i, j) -> map.[i, j] = '#')
    |> Seq.length

let step getOccupied limit map =
    let mutable changed = false

    let next =
        Array2D.mapi (fun x y ->
            function
            | '.' -> '.'
            | 'L' when getOccupied map x y = 0 ->
                changed <- true
                '#'
            | '#' when getOccupied map x y >= limit ->
                changed <- true
                'L'
            | x -> x) map

    (next, changed)

let run getOccupied limit map =
    let mutable next = (map, true)
    while snd next do
        next <- step getOccupied limit <| fst next

    fst next

let countOccupied = Seq.cast<char> >> count '#'

let solve =
    parse >> run occupiedAdjacents 4 >> countOccupied

solve testData |> print

submit 2020 11 Level.One <| solve data

// Level 2
let rec longDir map (x, y) (dx, dy) =
    seq {
        let p = (x + dx, y + dy)
        if seatExists map p then
            yield p
            yield! longDir map p (dx, dy)
    }

let findFirstSeat map p d =
    longDir map p d
    |> Seq.skipWhile (fun (x, y) -> map.[x, y] = '.')
    |> Seq.tryHead

let adjacents2 map x y =
    directions |> mapOption (findFirstSeat map (x, y))

let occupiedAdjacents2 map x y =
    adjacents2 map x y
    |> Seq.filter (fun (i, j) -> map.[i, j] = '#')
    |> Seq.length

let solve2 =
    parse >> run occupiedAdjacents2 5 >> countOccupied

solve2 testData |> print

submit 2020 11 Level.Two <| solve2 data