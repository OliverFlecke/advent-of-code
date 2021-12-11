#r "src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open System
open AdventOfCode.Utils

type SparseMatrix<'a> = Map<(int * int), 'a>

let bounds (matrix: SparseMatrix<'a>) =
    matrix
    |> Map.keys
    |> Seq.fold
        (fun (x_min, x_max, y_min, y_max) (x, y) -> (min x_min x, max x_max x, min y_min y, max y_max y))
        (Int32.MaxValue, Int32.MinValue, Int32.MaxValue, Int32.MinValue)

let indexes (x_min, y_min) (x_max, y_max) =
    seq {
        for y in y_min .. y_max do
            for x in x_min .. x_max do
                yield x, y
    }

module SparseMatrix =
    let empty: SparseMatrix<'a> = Map.empty

    let add pos value = Map.add (pos, value)

    let print mapping (matrix: SparseMatrix<'a>) =
        let x_min, x_max, y_min, y_max = bounds matrix

        for y in y_min .. y_max do
            for x in x_min .. x_max do
                Map.tryFind (x, y) matrix
                |> mapping
                |> printf "%s"

            printfn ""

    let map mapping matrix : SparseMatrix<'a> =
        let x_min, x_max, y_min, y_max = bounds matrix

        indexes (x_min - 1, y_min - 1) (x_max + 1, y_max + 1)
        |> Seq.fold
            (fun mx pos ->
                Map.tryFind pos matrix
                |> mapping pos
                |> fun v ->
                    match v with
                    | None -> mx
                    | Some x -> Map.add pos x mx)
            Map.empty

let aliveNeighbors matrix pos =
    directions
    |> Seq.map (fun (x, y) -> (fst pos + x, snd pos + y))
    |> Seq.map (flip Map.tryFind matrix)
    |> filterOption
    |> Seq.length

let gof matrix pos value =
    match value with
    | None ->
        if aliveNeighbors matrix pos = 3 then
            Some true
        else
            None

    | Some _ ->
        let alive = aliveNeighbors matrix pos

        if alive < 2 then None
        elif 3 < alive then None
        else Some true

let stringify x = if Option.isSome x then "#" else " "

let initial =
    set [ (0, 0); (0, 1); (0, 2) ]
    // I'm using the SparseMatrix here, even though a simple set would be enough. I wanted to build the SparseMatrix for later use, where it might be necessary to store values
    |> Set.map (fun x -> (x, true))
    |> Map.ofSeq

Seq.initInfinite id
|> Seq.scan (fun mx _ -> SparseMatrix.map (gof mx) mx) initial
// |> Seq.take 5
|> Seq.iter (fun mx ->
    SparseMatrix.print stringify mx
    printfn "-----"
    System.Threading.Thread.Sleep(500))
