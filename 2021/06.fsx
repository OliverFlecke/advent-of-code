#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2021 6
let testData = testInput 2021 6

let parse =
    splitComma >> Array.map int >> List.ofArray

let solve days fish =
    let groups =
        fish
        |> List.groupBy id
        |> List.map (fun (day, bin) -> (day, List.length bin |> uint64))

    let combine =
        List.groupBy fst
        >> List.map (fun (day, list) -> (day, List.sumBy snd list))

    // Can't simply use remainder, as newborns are larger than 6
    let nextDay x = if x = 0 then 6 else x - 1

    let rec run days fish =
        if days = 0 then
            fish
        else
            // printfn "Day: %d - %s" days
            // <| (List.map string >> String.concat ",") fish

            let born =
                match fish |> List.tryFind (fst >> (=) 0) with
                | None -> 0UL
                | Some (_, amount) -> amount

            let fish' =
                fish
                |> List.map (fun (d, amount) -> (nextDay d, amount))

            run (days - 1) <| combine fish' @ [ (8, born) ]

    run days groups |> List.sumBy snd

let solver days = parse >> solve days

testSolution Level.One 26UL <| solver 18 testData

testSolution Level.One 5934UL
<| solver 80 testData

submit 2021 6 Level.One <| solver 80 data

// Part B
testSolution Level.Two 26984457539UL
<| solver 256 testData

submit 2021 6 Level.Two <| solver 256 data
