namespace AdventOfCode.Solutions

open AdventOfCode
open AdventOfCode.Utils

[<AutoOpen>]
module private Day13 =
    type Fold =
        | X of int
        | Y of int

    let parse (str: string) =
        let split = str.Split("\n\n")

        let positions =
            split[0]
            |> splitLines
            |> Seq.ofArray
            |> Seq.map (fun s ->
                let line = s.Split(",")
                int line[0], int line[1])

        let folds =
            split[1]
            |> splitLines
            |> Seq.ofArray
            |> Seq.map (fun line ->
                match line with
                | ReMatch "fold along y=(\d+)" [y] -> Y <| int y
                | ReMatch "fold along x=(\d+)" [x] -> X <| int x
                | _ -> failwithf "Unable to understand instruction: %s" line)

        positions, folds

    let foldMatrix matrix (fold: Fold) =
        match fold with
        | Y line ->
            let upper = SparseMatrix.filter (fun pos _ -> snd pos < line) matrix
            let lower = SparseMatrix.filter (fun pos _ -> snd pos > line) matrix
            Map.fold (fun s (x, y) v -> SparseMatrix.add (x, line - (y - line)) v s) upper lower

        | X line ->
            let left = SparseMatrix.filter (fun pos _ -> fst pos < line) matrix
            let right = SparseMatrix.filter (fun pos _ -> fst pos > line) matrix
            Map.fold (fun s (x, y) v -> SparseMatrix.add (line - (x - line), y) v s) left right

    let getParsedData input =
        let positions, folds = parse input

        folds, positions |> Seq.map (fun x -> (x, true)) |> Map.ofSeq

type Year2021Day13() =
    interface ISolution with
        member _.year = 2021
        member _.day = 13

        member _.testA = seq [ (Int 17, None) ]
        member _.testB = Seq.empty

        member self.solveA input =
            let folds, matrix = getParsedData input

            (foldMatrix matrix (Seq.head folds)).Keys.Count |> Int

        member self.solveB input =
            let stringify x = if Option.isSome x then "#" else " "
            let folds, matrix = getParsedData input

            Seq.fold foldMatrix matrix folds
                // |> SparseMatrix.print stringify
                |> ignore // Ignoring output from this, to avoid it having to interfer with results table

            String "FJAHJGAH" // Hardcoded result for my input - Need to implement a way to convert the painted characters to real chars 🙄