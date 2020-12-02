namespace AdventOfCode

module Utils =
    let splitSpaces (str: string) = str.Split(" ")
    let splitLines (str: string) = str.Split("\n")

    let ints (s: string) = s.Trim() |> splitLines |> Seq.map int

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
