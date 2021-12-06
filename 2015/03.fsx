#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open AdventOfCode.Core

let data = input 2015 3
let parse (str: string) = Seq.toList str

let next (x, y) =
    function
    | '>' -> (x + 1, y)
    | '<' -> (x - 1, y)
    | '^' -> (x, y + 1)
    | 'v' -> (x, y - 1)
    | c -> failwith $"Unexpected character '{c}'"

let rec coordinates start next inst =
    seq {
        yield start
        match inst with
        | [] -> ()
        | h :: t -> yield! coordinates (next start h) next t
    }

let solve =
    parse
    >> coordinates (0, 0) next
    >> Seq.distinct
    >> Seq.length

let testData =
    seq {
        ">"
        "^>v<"
        "^v^v^v^v^v"
    }

testData |> Seq.iter (solve >> printfn "%d")

submit 2015 3 Level.One <| solve data

// Level 2
let rec coordinates2 santa robot next inst =
    seq {
        yield santa
        yield robot
        match inst with
        | [] -> ()
        | h :: [] -> yield next santa h
        | a :: b :: tail -> yield! coordinates2 (next santa a) (next robot b) next tail
    }

let solve2 =
    parse
    >> coordinates2 (0, 0) (0, 0) next
    >> Seq.distinct
    >> Seq.length

submit 2015 3 Level.Two <| solve2 data
