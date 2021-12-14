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

if not <| args.Contains Parser.Test_Data then
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
else
    let test level (s: ISolution) =
        let f, tests =
            if level = Level.One then
                s.solveA, s.testA
            else
                s.solveB, s.testB

        tests
        |> Seq.map (fun (expected, input) ->
            expected,
            f (
                input
                |> Option.defaultValue (testInput s.year s.day)
            ))
        |> Seq.iter (fun (expected, actual) -> Utils.testSolution level expected actual)

    let execute (s: ISolution) =
        printfn "Running tests for %i/%i" s.year s.day
        test Level.One s
        test Level.Two s

    solutions |> Seq.iter execute
