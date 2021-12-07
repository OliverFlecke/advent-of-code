#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open System.Text.RegularExpressions
open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2016 7
// let testData = testInput 2016 7

let abbaInBrackets str =
    match str with
    | ReMatch "\[\w*(\w)(\w)\2\1\w*\]" [ a; b ] when a <> b -> true
    | _ -> false

let abba str =
    let str' =
        Regex.Replace(str, "\[\w*(\w)(\w)\2\1\w*\]", "")

    match str' with
    | ReMatch "(\w)(\w)\2\1" [ a; b ] when a <> b -> true
    | _ -> false

let solve =
    splitLines
    >> Array.filter (abbaInBrackets >> not)
    >> Array.filter abba
    >> Array.length

submit 2016 7 Level.One <| solve data

// Part B
let supportsSSL str =
    let checkHypernets a b =
        Regex.Matches(str, "\[(\w*)\]")
        |> Seq.map (fun x -> x.Value)
        |> Seq.exists (fun s -> s.IndexOf($"{b}{a}{b}") <> -1)

    Regex.Matches(Regex.Replace(str, "\[\w*\]", ""), "(?=((.)(?!\2).\2))")
    |> Seq.map (fun x -> (x.Groups.[1].Value.[0], x.Groups.[1].Value.[1]))
    |> Seq.filter (fun (a, b) -> a <> b)
    |> Seq.filter (uncurry checkHypernets)
    |> Seq.exists (fun _ -> true)

let solve' =
    splitLines
    >> Array.filter supportsSSL
    >> Array.length

submit 2016 7 Level.Two <| solve' data
