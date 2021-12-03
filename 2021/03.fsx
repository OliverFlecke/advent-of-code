#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2021 3

let testData =
    "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"

let parse = splitLines >> List.ofArray

let count index (strs: string list) =
    let rec helper index c (strs: string list) =
        match strs with
        | [] -> c
        | str :: strs' ->
            match str.[index] with
            | '1' -> helper index (c + 1) strs'
            | _ -> helper index c strs'

    helper index 0 strs

let binaryStringToDecimal str = System.Convert.ToInt32(str, 2)

let solve data =
    let strs = parse data
    let bitsize = strs.Head.Length - 1

    let gamma =
        List.map (fun i -> count i strs) [ 0 .. bitsize ]
        |> List.map (fun x ->
            if x >= strs.Length - x then
                "1"
            else
                "0")
        |> Seq.ofList
        |> String.concat ""
        |> binaryStringToDecimal

    let eplison = ((1 <<< bitsize + 1) - 1) ^^^ gamma
    gamma * eplison

testSolution Level.One 198 <| solve testData

submit 2021 3 Level.One <| solve data

// Part B
let mostCommon index strs =
    let c = count index strs

    if c >= strs.Length - c then
        '1'
    else
        '0'

let solve' (strs: string list) =
    let rec helper predicate index strs =
        match strs with
        | [] -> failwith "There should always be one number left"
        | x :: [] -> x
        | _ ->
            let common = mostCommon index strs

            let strs' =
                List.filter (fun (str: string) -> predicate str.[index] common) strs

            helper predicate (index + 1) strs'

    let oxygen =
        helper (=) 0 strs |> binaryStringToDecimal

    let co2 =
        helper (<>) 0 strs |> binaryStringToDecimal

    oxygen * co2

testSolution Level.Two 230
<| (parse >> solve') testData

submit 2021 3 Level.Two <| (parse >> solve') data
