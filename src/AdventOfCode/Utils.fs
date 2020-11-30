namespace AdventOfCode

module Utils =
    let ints (s: string) = s.Trim().Split("\n") |> Seq.map int