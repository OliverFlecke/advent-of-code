module Timing

open System
open System.Diagnostics
open AdventOfCode

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

    let mutable totalTime = TimeSpan.Zero
    let mutable totalA = TimeSpan.Zero
    let mutable totalB = TimeSpan.Zero

    for r in results do
        printfn
            "Solution %i/%-2i | %10s | %10.1f µs | %10s | %10.1f µs | %10.1f µs"
            r.year
            r.day
            r.a.result
            (toMicro r.a.time)
            r.b.result
            (toMicro r.b.time)
            (toMicro (r.a.time + r.b.time))

        totalA <- totalA + r.a.time
        totalB <- totalB + r.b.time
        totalTime <- totalTime + (r.a.time + r.b.time)

    printColor ConsoleColor.Blue
    <| sprintf
        "Totals           |            | %10.1f µs |            | %10.1f µs | %10.1f µs"
        (toMicro totalA)
        (toMicro totalB)
        (toMicro totalTime)
