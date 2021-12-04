#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2021 4
let testData = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7"

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

let solve data =
    let (numbers, boards) = parse data
    let rec run boards numbers =
        let boards' = Array.map (markBoard <| Array.head numbers) boards
        match Array.tryFind hasWon boards' with
        | Some board -> (sumOfRemaining board, Array.head numbers)
        | None -> run boards' <| Array.tail numbers

    let (sum, lastNumber) = run boards numbers

    sum * lastNumber

testSolution Level.One 4512 <| solve testData

submit 2021 4 Level.One <| solve data

// Part B
let solve' data =
    let (numbers, boards) = parse data
    let rec run boards numbers =
        let markedBoards =
            boards
                |> Array.map (markBoard <| Array.head numbers)

        match Array.filter (hasWon >> not) markedBoards with
        | [| |] -> (sumOfRemaining markedBoards[0], Array.head numbers)
        | remainingBoards -> run remainingBoards <| Array.tail numbers

    let (sum, lastNumber) = run boards numbers

    sum * lastNumber

testSolution Level.Two 1924 <| solve' testData

submit 2021 4 Level.Two <| solve' data