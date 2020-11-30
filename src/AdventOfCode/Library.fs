namespace AdventOfCode

open System
open System.IO
open FSharp.Data
open System.Text
open System.Text.Json
open System.Collections.Generic

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

    let url year day =
        $"https://adventofcode.com/{year}/day/{day}"

    let inputUrl year day = url year day + "/input"
    let answerUrl year day = url year day + "/answer"
    let cookies = [ "session", token ]

    let input year day =
        let path =
            Path.Join("input", string year, $"{day}.txt")

        if File.Exists path then
            File.ReadAllText path
        else
            let content =
                Http.RequestString(inputUrl year day, httpMethod = "GET", cookies = cookies)

            File.WriteAllText(path, content, Encoding.UTF8)
            content

    type Level =
        | One = 1
        | Two = 2

    let private localResults filename =
        if File.Exists(filename) then
            File.ReadAllText(filename)
            |> JsonSerializer.Deserialize<Dictionary<string, Level>>
        else
            new Dictionary<string, Level>()

    let markAsSolved year day level =
        let filename = $"{year}.json"
        let results = localResults filename

        results.Item($"day{day}") <- level

        File.WriteAllText(filename, JsonSerializer.Serialize(results), Encoding.UTF8)

    let checkIfSolved year day (level: Level) =
        let results = localResults $"{year}.json"
        let key = $"day{day}"

        results.ContainsKey(key)
        && results.Item($"day{day}")
        >= level

    let submitToServer year day (level: Level) answer =
        printfn $"Submitting '{answer}' for problem '{year}/{day}' level '{level}'"

        let data =
            [ "level", string level
              "answer", string answer ]

        let response =
            Http.RequestString(answerUrl year day, body = FormValues data, httpMethod = "POST", cookies = cookies)

        let correct =
            match response with
            | _ when response.Contains("that's the right answer") ->
                pSuccess "You answered correctly!"
                true
            | _ when response.Contains("already complete it") ->
                pSuccess "You already completed this problem"
                true
            | _ when response.Contains("answer too recently") ->
                pWarn "You have an answer to recently. Wait a bit and try again."
                false
            | _ when response.Contains("not the right answer") ->
                pError "You answered incorrectly! Try again"
                printfn $"{response}"
                false
            | _ ->
                pWarn "Unknown response"
                printfn $"{response}"
                false

        correct

    let submit (year: int) (day: int) (level: Level) answer =
        if checkIfSolved year day level then
            pSuccess $"You already have the star for {year} day {day}. Skipping submission"
            true
        else
            let isCorrect = submitToServer year day level answer
            if isCorrect then markAsSolved year day level
            isCorrect
