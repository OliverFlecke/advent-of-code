namespace AdventOfCode.Solutions

open AdventOfCode
open AdventOfCode.Types
open AdventOfCode.Utils

[<AutoOpen>]
module private Day10 =
    let parse = splitLines >> List.ofArray

    let points c =
        match c with
        | ')' -> 3
        | ']' -> 57
        | '}' -> 1197
        | '>' -> 25137
        | _ -> failwith $"Unable to get point for '{c}'"

    let closing (c: char) =
        c = '}' || c = ')' || c = ']' || c = '>'

    let opposite c =
        match c with
        | '{' -> '}'
        | '(' -> ')'
        | '[' -> ']'
        | '<' -> '>'
        | _ -> failwith $"Unsupported character '{c}'"

    let analyze str =
        let rec analyze' parsed rest =
            match parsed, rest with
            | _, [] -> Left <| Seq.map opposite parsed
            | p :: _, n :: _ when closing n && opposite p <> n -> Right n
            | _ :: parsed', n :: rest' when closing n -> analyze' parsed' rest'
            | _, n :: rest' -> analyze' (n :: parsed) rest'

        analyze' [] str

    let solve =
        List.map (List.ofSeq >> analyze)
        >> Seq.filter Either.isRight
        >> Seq.map Either.getRight
        >> Seq.sumBy points

    let solver = parse >> solve

[<AutoOpen>]
module private Day10B =
    let points' c =
        match c with
        | ')' -> 1
        | ']' -> 2
        | '}' -> 3
        | '>' -> 4
        | _ -> failwith "Unsupported point"

    let solve' input =
        let values =
            input
            |> (List.map (List.ofSeq >> analyze)
                >> Seq.filter Either.isLeft
                >> Seq.map (
                    Either.getLeft
                    >> Seq.map (points' >> int64)
                    >> Seq.reduce (fun acc v -> acc * 5L + v)
                )
                >> List.ofSeq
                >> List.sort)

        values.[values.Length / 2]

    let solver' = parse >> solve'

type Year2021Day10() =
    interface ISolution with
        member _.year = 2021
        member _.day = 10

        member _.testA = seq [ (Int 26397, None) ]
        member _.testB = seq [ (Int64 288957L, None) ]

        member _.solveA input = Int <| solver input

        member _.solveB input = Int64 <| solver' input
