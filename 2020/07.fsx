#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Debug/net5.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils
open System.Text.RegularExpressions
open System.Collections.Generic

let data = input 2020 7
let testData = testInput 2020 7

let sampleLine =
    "light red bags contain 1 bright white bag, 2 muted yellow bags."

let parseContent (str: string): Option<list<int * string>> =
    match str with
    | "no other bags." -> None
    | _ ->
        Some
            (str.Split(",")
             |> Seq.map (function
                 | ReMatch @"(\d+) (.*) bags?" [ _; count; name ] -> (int count.Value, name.Value)
                 | x -> failwith $"Unmatch pattern '{x}'")
             |> Seq.toList)


let parseLine (str: string) =
    let m =
        Regex.Match(str, @"(?<key>.*) bags contain (?<content>.*)")

    let key = m.Groups.["key"].Value

    let bags =
        match parseContent m.Groups.["content"].Value with
        | Some cs -> cs
        | None -> []

    (key, bags)

type Rules = seq<string * (int * string) list>
let parse: string -> Rules = splitLines >> Seq.map parseLine

// parseLine sampleLine |> string |> printfn "%s"

// parse testData
// |> Seq.iter (fun x -> printfn $"{x}")

let rec ancestors (rules: Rules) (key: string) =
    let parents =
        rules
        |> Seq.filter (fun (_, vs) -> vs |> Seq.map snd |> Seq.contains key)
        |> Seq.map fst
        |> Seq.toList

    key :: List.collect (ancestors rules) parents

let myBag = "shiny gold"

let solve =
    parse
    >> (fun rules -> ancestors rules myBag)
    >> Seq.filter ((<>) myBag)
    >> Seq.distinct
    >> Seq.length

solve testData
submit 2020 7 Level.One <| solve data

// Level 2
let testData2 = """shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags."""

type Dict = IDictionary<string, (int * string) list>

let rec count (rules: Dict) bag =
    match rules.[bag] with
    | [] -> 1
    | rest ->
        1
        + Seq.sumBy (fun (c, k) -> c * (count rules k)) rest

let solve2 =
    parse
    >> dict
    >> fun rs -> count rs myBag
    >> fun c -> c - 1

// solve2 testData |> printfn "%d"
// solve2 testData2 |> printfn "%d"

submit 2020 7 Level.Two <| solve2 data
