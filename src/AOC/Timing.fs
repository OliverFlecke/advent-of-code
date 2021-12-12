module Timing

open System.Diagnostics

type Timed = { result: string; time: int64 }

type TimedResult =
    { year: int
      day: int
      a: Timed
      b: Timed
      parseTime: int64 }

let timeit f : Timed =
    let timer = new Stopwatch()
    timer.Start()
    let res = f ()
    timer.Stop()

    { result = res
      time = timer.ElapsedMilliseconds }

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
