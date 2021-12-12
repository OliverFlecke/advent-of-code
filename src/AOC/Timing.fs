module Timing

open System
open System.Diagnostics

type Timed = { result: string; time: TimeSpan }

type TimedResult =
    { year: int
      day: int
      a: Timed
      b: Timed }

let toMicro (timespan: TimeSpan) = float timespan.Ticks / 10.0

let timeit f : Timed =
    let timer = new Stopwatch()
    timer.Start()
    let res = f ()
    timer.Stop()

    { result = res
      time = TimeSpan.FromTicks timer.ElapsedTicks }

let printTable results =
    printfn "                 | Answer A   | Time A        | Answer B   | Time B        | Total time"

    for result in results do
        printfn
            "Solution %i/%-2i | %10s | %10.1f µs | %10s | %10.1f µs |  %10.1f µs"
            result.year
            result.day
            result.a.result
            (toMicro result.a.time)
            result.b.result
            (toMicro result.b.time)
            (toMicro (result.a.time + result.b.time))
