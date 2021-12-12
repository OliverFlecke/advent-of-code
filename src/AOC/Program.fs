printfn "*** AOC solutions ***\n"

open System
open System.Reflection
open AdventOfCode

open System.Diagnostics

let solutions =
    Assembly.GetExecutingAssembly().GetTypes()
    |> Seq.filter (fun x ->
        x.GetInterfaces()
        |> Seq.exists (fun i -> i = typeof<ISolution>))
    |> Seq.map (fun t -> Activator.CreateInstance(t) :?> ISolution)

type Timed = { result: string; time: int64 }

let timeit f : Timed =
    let timer = new Stopwatch()
    timer.Start()
    let res = f ()
    timer.Stop()

    { result = res
      time = timer.ElapsedMilliseconds }

type TimedResult =
    { year: int
      day: int
      a: Timed
      b: Timed
      parseTime: int64 }

let execute (s: ISolution) : TimedResult =
    let parsed = timeit (fun () -> input s.year s.day)

    let a =
        timeit (fun () -> s.solveA parsed.result)

    let b =
        timeit (fun () -> s.solveB parsed.result)

    { year = s.year
      day = s.day
      a = a
      b = b
      parseTime = parsed.time }

let results = solutions |> Seq.map execute

let printTable results =
    for result in results do
        printfn
            "Solution %i/%2i | Part A: %10s %6i ms | Part B: %10s %6i ms | Parsing %10i ms  Total: %10i ms"
            result.year
            result.day
            result.a.result
            result.a.time
            result.b.result
            result.b.time
            result.parseTime
            (result.a.time + result.b.time + result.parseTime)

do printTable results

let doSubmit = true

if doSubmit then
    for r in results do
        submit r.year r.day Level.One r.a.result |> ignore
        submit r.year r.day Level.Two r.b.result |> ignore
