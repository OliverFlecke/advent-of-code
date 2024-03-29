﻿namespace AdventOfCode

open System
open System.IO
open FSharp.Data
open System.Text
open System.Text.Json
open System.Text.RegularExpressions
open System.Collections.Generic

[<AutoOpen>]
module Core =
    let token =
        System.Environment.GetEnvironmentVariable("AOC_TOKEN")

    let printColor color msg =
        Console.ForegroundColor <- color
        printfn $"{msg}"
        Console.ForegroundColor <- ConsoleColor.White

    let pWarn = printColor ConsoleColor.Yellow
    let pError = printColor ConsoleColor.Red
    let pSuccess = printColor ConsoleColor.Green

    let private url year day =
        $"https://adventofcode.com/{year}/day/{day}"

    let inputUrl year day = url year day + "/input"
    let answerUrl year day = url year day + "/answer"
    let cookies = [ "session", token ]

    let input year day =
        let dir = Path.Join [| "input"; string year |]
        let path = Path.Join [| dir; $"{day}.txt" |]
        Directory.CreateDirectory dir |> ignore

        let content =
            if File.Exists path then
                File.ReadAllText path
            else
                let content =
                    Http.RequestString(inputUrl year day, httpMethod = "GET", cookies = cookies)

                File.WriteAllText(path, content, Encoding.UTF8)
                content

        content.Trim()

    let testInput year day =
        let dir =
            Path.Join [| "input"
                         "test"
                         string year |]

        let path = Path.Join [| dir; $"{day}.txt" |]

        if File.Exists path then
            File.ReadAllText path
        else
            pError $"Test file could not be found. Path: '{path}'"
            exit 1

    type Level =
        | One = 1
        | Two = 2

    let private localResults filename =
        if File.Exists(filename) then
            File.ReadAllText(filename)
            |> JsonSerializer.Deserialize<Dictionary<string, Level>>
        else
            new Dictionary<string, Level>()

    let private dayKey (day: int) = $"day{day}"
    let private starDir = ".stars"

    let markAsSolved (year: int) (day: int) (level: Level) =
        Directory.CreateDirectory starDir |> ignore
        let filename = Path.Join [| starDir; $"{year}.json" |]
        let results = localResults filename

        results.Item(dayKey day) <- level

        File.WriteAllText(filename, JsonSerializer.Serialize(results), Encoding.UTF8)

    let checkIfSolved year day (level: Level) =
        let results =
            localResults
            <| Path.Join [| starDir; $"{year}.json" |]

        results.ContainsKey(dayKey day)
        && results.Item(dayKey day) >= level

    let submitToServer year day (level: Level) answer =
        printfn $"Submitting '{answer}' for problem '{year}/{day}' level '{level}'"

        let data =
            [ "level", string <| int level
              "answer", string answer ]

        let response =
            Http.RequestString(answerUrl year day, body = FormValues data, httpMethod = "POST", cookies = cookies)

        match response with
        | _ when response.Contains("That's the right answer") ->
            pSuccess "You answered correctly!"
            true
        | _ when response.Contains("already complete it") ->
            pSuccess "You already completed this problem"
            true
        | _ when response.Contains("answer too recently") ->
            pWarn "You have an answer to recently. Wait a bit and try again."

            let m =
                Regex.Match(response, @"(?<time>You have \d+s left to wait)")

            if m.Success then
                pWarn m.Groups.["time"].Value

            false
        | _ when response.Contains("not the right answer") ->
            pError "You answered incorrectly! Try again"
            false
        | _ ->
            pWarn "Unknown response"
            printfn $"{response}"
            false

    let submit (year: int) (day: int) (level: Level) answer =
        if checkIfSolved year day level then
            pSuccess $"You already have the star for {year}/{day} level {level}. Skipping submission (answer: {answer})"
            true
        else
            let isCorrect = submitToServer year day level answer

            if isCorrect then
                markAsSolved year day level

            isCorrect
