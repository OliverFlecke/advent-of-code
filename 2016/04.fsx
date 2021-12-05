#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open System.Text.RegularExpressions
open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2016 4
let testData = testInput 2016 4

let parseRoom str =
    let m = Regex.Match(str, "(?<name>.*)-(?<id>\d+)\[(?<checksum>[a-z]{5})\]")
    if m.Success
        then (m.Groups["name"].Value, int m.Groups["id"].Value, m.Groups["checksum"].Value)
        else failwith $"Unable to parse {str}"

let parse = splitLines >> Array.map parseRoom

let order =
    Seq.groupBy snd
    >> Seq.sortBy fst
    >> Seq.rev
    >> Seq.map (snd >> Seq.map fst >> Seq.sort)
    >> Seq.collect id
    >> Seq.map string
    >> Seq.take 5
    >> String.concat ""

type Room = string * int * string

let checksum str =
    let count c = Seq.filter ((=) c) >> Seq.length

    str
        |> Set.ofSeq
        |> Set.filter ((<>) '-')
        |> Set.map (fun c -> (c, count c str))
        |> order

let checkRoom (name, _, c) = checksum name = c

let solve (rooms: seq<Room>) =
    rooms
        |> Seq.filter checkRoom
        |> Seq.sumBy (fun (_, id, _) -> id)

let solver = parse >> solve

testSolution Level.One 1514 <| solver testData
submit 2016 4 Level.One <| solver data

// Part B
let shift amount (c: char) =
    if c = '-'
    then ' '
    else
        let a = int 'a'
        let m = int 'z' - int 'a' + 1

        ((int c - a) + amount) % m |> ((+) a) |> char

let transform n id = Seq.map (shift id) n |> Seq.map string |> String.concat ""

let solve' (rooms: seq<Room>) =
    rooms
        |> Seq.filter checkRoom
        |> Seq.map (fun (n, id, _) -> (transform n id, id))

let solver' =
    parse
    >> solve'
    >> Seq.find (fst >> (=) "northpole object storage")
    >> snd

submit 2016 4 Level.Two <| solver' data
