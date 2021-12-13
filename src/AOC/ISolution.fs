namespace AdventOfCode

type SolutionResult =
    | String of string
    | Int of int
    | Int64 of int64

    override x.ToString() =
        match x with
        | String str -> str
        | Int i -> string i
        | Int64 i -> string i

type public ISolution =
    abstract year : int
    abstract day : int

    abstract solveA : string -> SolutionResult

    abstract solveB : string -> SolutionResult

module Solution =
    open System
    open System.Reflection
    open Argu
    open Parser

    let getSolutions (args: ParseResults<CliArguments>) =
        Assembly.GetExecutingAssembly().GetTypes()
        |> Seq.filter (fun x ->
            x.GetInterfaces()
            |> Seq.exists (fun i -> i = typeof<ISolution>))
        |> Seq.map (fun t -> Activator.CreateInstance(t) :?> ISolution)
        |> Seq.filter (fun s ->
            args.TryGetResult(Year)
            |> Option.map ((=) s.year)
            |> Option.defaultValue true)
        |> Seq.filter (fun s ->
            args.TryGetResult(Day)
            |> Option.map ((=) s.day)
            |> Option.defaultValue true)
