namespace AdventOfCode2020

open FSharp.Data

module Utils =
    // TODO: Better way to find token
    let session =
        System.IO.File.ReadAllText "../../../../.token"

    let url year day =
        $"https://adventofcode.com/{year}/day/{day}/input"

    let getProblem year day =
        Http.RequestString(url year day, httpMethod = "GET", cookies = [ "session", session ])
