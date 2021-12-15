namespace AdventOfCode.Solutions

open AdventOfCode
open AdventOfCode.Utils

[<AutoOpen>]
module private Day4 =
    type Board = int option[,]

    let parseBoard (str: string): Board =
        let numbers =
            str.ReplaceLineEndings(" ")
                |> splitSpaces
                |> Array.map trim
                |> Array.filter (fun x -> x <> "")
                |> Array.map (int >> Some)
        array2D [
            numbers[0..4];
            numbers[5..9];
            numbers[10..14];
            numbers[15..19];
            numbers[20..24];
        ]

    let parse (str: string) =
        let splits = str.Split("\n\n")
        let numbers = splits[0].Split(",") |> Array.map int

        (numbers, Array.map parseBoard splits[1..])

    let markBoard number =
        Array2D.map (fun x ->
            match x with
            | Some n -> if n = number then None else x
            | None -> None)

    let hasWon (board: Board) =
        let checkRow = Array.exists Option.isSome >> not
        let rec helper i =
            if i < 0 then false
            elif checkRow board[*, i] || checkRow board[i, *] then true
            else helper <| i - 1

        helper (Array2D.length1 board - 1)

    let sumOfRemaining (board: Board) =
        (Seq.cast<int option>
            >> Seq.filter ((<>) None)
            >> Seq.map Option.get
            >> List.ofSeq
            >> List.sum) board

type Year2021Day4() =
    interface ISolution with
        member _.year = 2021
        member _.day = 4

        member _.testA = seq [ (Int 4512, None) ]
        member _.testB = seq [ (Int 1924, None) ]

        member self.solveA data =
            let (numbers, boards) = parse data
            let rec run boards numbers =
                let boards' = Array.map (markBoard <| Array.head numbers) boards
                match Array.tryFind hasWon boards' with
                | Some board -> (sumOfRemaining board, Array.head numbers)
                | None -> run boards' <| Array.tail numbers

            let (sum, lastNumber) = run boards numbers

            Int <| sum * lastNumber

        member self.solveB data =
            let (numbers, boards) = parse data
            let rec run boards numbers =
                let markedBoards =
                    boards
                        |> Array.map (markBoard <| Array.head numbers)

                match Array.filter (hasWon >> not) markedBoards with
                | [| |] -> (sumOfRemaining markedBoards[0], Array.head numbers)
                | remainingBoards -> run remainingBoards <| Array.tail numbers

            let (sum, lastNumber) = run boards numbers

            Int <| sum * lastNumber