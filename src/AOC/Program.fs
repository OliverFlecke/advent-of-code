open System
open System.Reflection
open AdventOfCode
open Timing

let args = Parser.getArgs

if args.IsUsageRequested then
    Parser.parser.PrintUsage() |> printfn "%s"
    exit 0

printfn "*** AOC solutions ***\n"

let solutions =
    Assembly.GetExecutingAssembly().GetTypes()
    |> Seq.filter (fun x ->
        x.GetInterfaces()
        |> Seq.exists (fun i -> i = typeof<ISolution>))
    |> Seq.map (fun t -> Activator.CreateInstance(t) :?> ISolution)
    |> Seq.filter (fun s ->
        args.TryGetResult(Parser.Year)
        |> Option.map ((=) s.year)
        |> Option.defaultValue true)
    |> Seq.filter (fun s ->
        args.TryGetResult(Parser.Day)
        |> Option.map ((=) s.day)
        |> Option.defaultValue true)

if Seq.isEmpty solutions then
    pError "No solutions where found"
    exit 1

let execute (s: ISolution) : TimedResult =
    let data = input s.year s.day

    let a = timeit (fun () -> s.solveA data)

    let b = timeit (fun () -> s.solveB data)

    { year = s.year
      day = s.day
      a = a
      b = b }

let results = solutions |> Seq.map execute

for r in results do
    if args.Contains Parser.Submit_A then
        submit r.year r.day Level.One r.a.result |> ignore

    if args.Contains Parser.Submit_B then
        submit r.year r.day Level.Two r.b.result |> ignore

do printTable results
