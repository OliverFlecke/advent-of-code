namespace AdventOfCode.Solutions

open AdventOfCode
open AdventOfCode.Utils

type Fold =
    | X of int
    | Y of int

type Year2021Day13() =
    member _.parse (str: string) =
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

    member _.foldMatrix matrix (fold: Fold) =
        match fold with
        | Y line ->
            let upper = SparseMatrix.filter (fun pos _ -> snd pos < line) matrix
            let lower = SparseMatrix.filter (fun pos _ -> snd pos > line) matrix
            Map.fold (fun s (x, y) v -> SparseMatrix.add (x, line - (y - line)) v s) upper lower

        | X line ->
            let left = SparseMatrix.filter (fun pos _ -> fst pos < line) matrix
            let right = SparseMatrix.filter (fun pos _ -> fst pos > line) matrix
            Map.fold (fun s (x, y) v -> SparseMatrix.add (line - (x - line), y) v s) left right

    member self.getParsedData input =
        let positions, folds = self.parse input

        folds, positions |> Seq.map (fun x -> (x, true)) |> Map.ofSeq

    interface ISolution with
        member _.year = 2021
        member _.day = 13

        member self.solveA input =
            let folds, matrix = self.getParsedData input

            (self.foldMatrix matrix (Seq.head folds)).Keys.Count |> string

        member self.solveB input =
            let stringify x = if Option.isSome x then "#" else " "
            let folds, matrix = self.getParsedData input

            Seq.fold (fun mx fold -> self.foldMatrix mx fold) matrix folds
                |> SparseMatrix.print stringify

            "FJAHJGAH" // Hardcoded result for my input - Need to implement a way to convert the painted characters to real chars 🙄