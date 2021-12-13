namespace AdventOfCode

open System

type SparseMatrix<'a> = Map<(int * int), 'a>

module SparseMatrix =
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

    let empty: SparseMatrix<'a> = Map.empty

    let add pos value = Map.add pos value

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

    let filter predicate matrix = Map.filter predicate matrix
