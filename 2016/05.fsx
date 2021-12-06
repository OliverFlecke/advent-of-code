#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2016 5
let testData = testInput 2016 5

open System.Security.Cryptography
open System.Text

let md5 (message: string) =
    use hasher = MD5.Create()

    message
    |> Encoding.ASCII.GetBytes
    |> hasher.ComputeHash
    |> Seq.map (fun c -> c.ToString("X2"))
    |> Seq.reduce (+)

let solve input =
    Seq.initInfinite (fun i -> md5 <| input + string i)
        |> Seq.filter (fun str -> str.StartsWith("00000"))
        |> Seq.take 8
        |> Seq.map (fun str -> str.[5..5])
        |> Seq.reduce (+)
        |> toLower

testSolution Level.One "18f47a30" <| solve testData
submit 2016 5 Level.One <| solve data
