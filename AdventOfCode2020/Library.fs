namespace AdventOfCode2020

open System.IO;
open FSharp.Data
open System.Text

module Utils =
    // TODO: Better way to find token
    let session =
        System.IO.File.ReadAllText "../../../../.token"

    let url year day =
        $"https://adventofcode.com/{year}/day/{day}/input"

    let getProblem year day =
        let content = Http.RequestString(url year day, httpMethod = "GET", cookies = [ "session", session ])
        File.WriteAllText("input.txt", content, Encoding.UTF8)
        content
