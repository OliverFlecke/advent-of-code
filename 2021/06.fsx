#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2021 6
let testData = testInput 2021 6

let parse = splitComma >> List.ofArray >> List.map int

let solve days fish =
    let rec run days fish =
        if days = 0 then
            fish
        else
            print $"Day: {days}"
            let born = List.filter ((=) 0) >> List.length
            let fish' =
                List.map (fun x -> if x = 0 then 6 else x - 1) fish

            run (days - 1) (fish' @ List.init (born fish) (fun _ -> 8))

    run days fish
        |> List.length

let solver days = parse >> solve days

testSolution Level.One 26 <| solver 18 testData
testSolution Level.One 5934 <| solver 80 testData

submit 2021 6 Level.One <| solver 80 data

// Part B
testSolution Level.Two 26984457539L <| solver 256 testData