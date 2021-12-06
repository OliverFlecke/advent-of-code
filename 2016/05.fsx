#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2016 5
let testData = testInput 2016 5

let generator input =
    Seq.initInfinite (fun i -> md5 <| input + string i)
    |> Seq.filter (fun str -> str.StartsWith("00000"))

let solve input =
    generator input
    |> Seq.take 8
    |> Seq.map (fun str -> str.[5..5])
    |> Seq.reduce (+)
    |> toLower

// Execution of solutions have been commented out, as there are not "instantly" computed
// testSolution Level.One "18f47a30" <| solve testData
// submit 2016 5 Level.One <| solve data

// Part B
let solve' input =
    let password: string option [] = Array.init 8 (fun _ -> None)

    let setNumber (index, char) =
        match index with
        | Int i ->
            if i < 8 && password.[i] = None then
                printfn $"Found '{char}' at index: {i}"
                Array.set password i <| Some char

            ()
        | _ -> ()

    do
        generator input
        |> Seq.map (fun str -> (str.[5..5], str.[6..6]))
        |> Seq.map setNumber
        |> Seq.takeWhile (fun _ -> Array.exists ((=) None) password)
        |> Seq.toList
        |> ignore

    password
    |> Seq.map Option.get
    |> Seq.reduce (+)
    |> toLower

// testSolution Level.Two "05ace8e3" <| solve' testData
submit 2016 5 Level.Two <| solve' data
