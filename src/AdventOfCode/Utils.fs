namespace AdventOfCode

open System.Text.RegularExpressions
open AdventOfCode.Core

module Utils =
    let splitSpaces (str: string) = str.Split(" ")
    let splitLines (str: string) = str.Split("\n")

    let ints (s: string) = s.Trim() |> splitLines |> Seq.map int

    let count c = Seq.filter ((=) c) >> Seq.length
    let modulo m n = ((n % m) + m) % m
    let manhattan (x, y) = abs x + abs y

    let print x = printfn $"{x}"

    let testSolution (level: Level) expected actual =
        if expected = actual
            then pSuccess $"Level {level} passed"
            else pError $"Level {level} failed. Expected {expected}, got {actual}"

    let rec private combinationsImpl acc size set =
        seq {
            match size, set with
            | n, x :: xs ->
                if n > 0
                then yield! combinationsImpl (x :: acc) (n - 1) xs
                if n >= 0 then yield! combinationsImpl acc n xs
            | 0, [] -> yield acc
            | _, [] -> ()
        }

    let combinations n set = combinationsImpl [] n set

    let (|ReMatch|_|) pattern input =
        if isNull input then
            None
        else
            let m =
                Regex.Match(input, pattern, RegexOptions.Compiled)

            if m.Success then Some [ for x in m.Groups -> x ] else None

    let rec tails =
        function
        | [] -> []
        | _ :: xs -> xs :: tails xs

    let filterOption (ls: seq<'a option>) =
        seq {
            for x in ls do
                match x with
                | Some item -> yield item
                | None -> ()
        }

    let mapOption f = Seq.map f >> filterOption

    let directions =
        seq {
            for x = -1 to 1 do
                for y = -1 to 1 do
                    if x <> 0 || y <> 0 then yield (x, y)
        }
