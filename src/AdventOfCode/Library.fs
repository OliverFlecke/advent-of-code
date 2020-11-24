namespace AdventOfCode

open System
open System.IO
open FSharp.Data
open System.Text

module Utils =
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

    let getInput year day =
        let path =
            Path.Join("input", string year, $"{day}.txt")

        if File.Exists path then
            File.ReadAllText path
        else
            let content =
                Http.RequestString(inputUrl year day, httpMethod = "GET", cookies = cookies)

            File.WriteAllText(path, content, Encoding.UTF8)
            content

    let submit year day level answer =
        printfn $"Submitting '{answer}' for problem '{year}/{day}' level '{level}'"

        let data =
            [ "level", string level
              "answer", string answer ]

        let response =
            Http.RequestString(answerUrl year day, body = FormValues data, httpMethod = "POST", cookies = cookies)

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
