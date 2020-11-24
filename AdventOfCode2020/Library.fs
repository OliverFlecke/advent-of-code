namespace AdventOfCode2020

open FSharp.Data

module Utils =


    let url year day =
        $"https://adventofcode.com/{year}/day/{day}/input"

    let getProblem year day =
        Http.RequestString(url year day, httpMethod = "GET", cookies = [ "session", session ])
