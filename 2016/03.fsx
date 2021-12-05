#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2016 3

let parse =
    splitLines
    >> Array.map (
        splitSpaces
        >> (Array.map trim
            >> Array.filter ((<>) "")
            >> Array.map int)
    )

let checkTriangle line =
    match line with
    | [| a; b; c |] when a + b > c && a + c > b && b + c > a -> true
    | _ -> false

let solve =
    parse
    >> Array.filter checkTriangle
    >> Array.length

let a = solve data
submit 2016 3 Level.One a

// Part B
let parse' =
    parse
    >> take 3
    >> Seq.map (array2D >> transpose >> Seq.cast<int>)
    >> Seq.collect id
    >> take 3
    >> Seq.map Array.ofSeq

let solve' =
    parse' >> Seq.filter checkTriangle >> Seq.length

submit 2016 3 Level.Two <| solve' data
