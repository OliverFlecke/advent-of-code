module Parser

open System
open Argu

type CliArguments =
    | Submit_A
    | Submit_B
    | Year of int
    | Day of int

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Submit_A -> "Specify if solution for part A should be submitted"
            | Submit_B -> "Specify if solution for part B should be submitted"
            | Year _ -> "Year of solutions to run"
            | Day _ -> "Day of solutions to run"

let parser = ArgumentParser.Create<CliArguments>(programName = "advent-of-code")

let getArgs = parser.Parse(Environment.GetCommandLineArgs()[1..], raiseOnUsage = false)

