#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net5.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils
open System.Security.Cryptography
open System.Text

let data = input 2015 4 |> (fun s -> s.Trim())

let bytes (str: string) = Encoding.UTF8.GetBytes str

let hash str =
    str
    |> bytes
    |> MD5.HashData
    |> Seq.map (fun x -> x.ToString("x2"))
    |> Seq.reduce (+)

let rec generate seed i =
    seq {
        yield hash (seed + (string i))
        yield! generate seed (i + 1)
    }

let solve seed (pattern: string) =
    generate seed 0
    |> Seq.takeWhile (fun s -> not <| s.StartsWith(pattern))
    |> Seq.length

submit 2015 4 Level.One <| solve data "00000"
submit 2015 4 Level.Two <| solve data "000000"
