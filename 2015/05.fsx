#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net5.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils
open System.Text.RegularExpressions

let data = input 2015 5
let testData = testInput 2015 5

let parse = splitLines

let hasRepeat =
    Seq.pairwise >> Seq.exists (fun (a, b) -> a = b)

let isVowel (c: char) = "aeiou".Contains(c)

let hasThreeVowels (str: string) =
    str |> Seq.filter isVowel |> Seq.length >= 3

let badWords = [ "ab"; "cd"; "pq"; "xy" ]

let noBadWords (str: string) =
    not
    <| Seq.exists (fun (word: string) -> str.Contains(word)) badWords

let isNice str =
    let filters =
        [ hasRepeat
          hasThreeVowels
          noBadWords ]

    Seq.forall (fun filter -> filter str) filters

testData
|> (splitLines
    >> Seq.map isNice
    >> Seq.iter (printfn "%b"))

let solve = parse >> Seq.filter isNice >> Seq.length

submit 2015 5 Level.One <| solve data

// Level 2
let hasPair str = Regex.IsMatch(str, @"(..).*\1")

let hasRepeat2 =
    Seq.windowed 3
    >> Seq.exists (function
        | [| a; _; b |] -> a = b
        | _ -> false)

let isNice2 str = hasPair str && hasRepeat2 str

let solve2 =
    parse >> Seq.filter isNice2 >> Seq.length

submit 2015 5 Level.Two <| solve2 data
