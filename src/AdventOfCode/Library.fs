namespace AdventOfCode

open System.IO
open FSharp.Data
open System.Text

module Utils =
    let session path = System.IO.File.ReadAllText path

    let url year day =
        $"https://adventofcode.com/{year}/day/{day}/input"

    let getInput tokenPath year day =
        let path =
            Path.Join("input", string year, $"{day}.txt")

        if File.Exists path then
            File.ReadAllText path
        else
            let content =
                Http.RequestString(url year day, httpMethod = "GET", cookies = [ "session", session tokenPath ])

            File.WriteAllText(path, content, Encoding.UTF8)
            content
