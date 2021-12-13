open AdventOfCode
open Timing

let args = Parser.getArgs

if args.IsUsageRequested then
    Parser.parser.PrintUsage() |> printfn "%s"
    exit 0

let solutions = Solution.getSolutions args

if Seq.isEmpty solutions then
    pError "No solutions where found"
    exit 1

let execute (s: ISolution) : TimedResult =
    let data =
        if args.Contains Parser.Test_Data then
            testInput s.year s.day
        else
            input s.year s.day

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

if args.Contains Parser.Print_Table then
    printTable results
