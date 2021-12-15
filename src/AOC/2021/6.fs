namespace AdventOfCode.Solutions

open AdventOfCode
open AdventOfCode.Utils

[<AutoOpen>]
module private Day6 =
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

type Year2021Day6() =
    interface ISolution with
        member _.year = 2021
        member _.day = 6

        member _.testA = seq [ (UInt64 26UL, None) ]
        member _.testB = seq [ (UInt64 26984457539UL, None) ]

        member _.solveA input = UInt64 <| solver 18 input

        member _.solveB input = UInt64 <| solver 256 input
