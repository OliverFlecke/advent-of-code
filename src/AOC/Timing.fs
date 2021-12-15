module Timing

open System
open System.IO
open System.Diagnostics
open AdventOfCode

type Timed =
    { result: SolutionResult
      time: TimeSpan }

type TimedResult =
    { year: int
      day: int
      a: Timed
      b: Timed }

let formatTimeSpan (timespan: TimeSpan) = timespan.ToString("mm\:ss\.ffffff").PadLeft(15)

let timeit f : Timed =
    let timer = new Stopwatch()
    timer.Start()
    let res = f ()
    timer.Stop()

    { result = res
      time = timer.Elapsed }

let printTable results =
    let writer = Console.Out

    printColor ConsoleColor.Blue
    <| sprintf "        |     Answer A     |     Time A      |     Answer B     |     Time B      |   Total time"

    let mutable totalA = TimeSpan.Zero
    let mutable totalB = TimeSpan.Zero

    // Suppress the output writen inside the solutions when outputting the table.
    Console.SetOut(TextWriter.Null)

    for r in results do

        Console.SetOut(writer)

        printfn
            "%i/%-2i | %16s | %s | %16s | %s | %s"
            r.year
            r.day
            (r.a.result.ToString())
            (formatTimeSpan r.a.time)
            (r.b.result.ToString())
            (formatTimeSpan r.b.time)
            (formatTimeSpan (r.a.time + r.b.time))

        totalA <- totalA + r.a.time
        totalB <- totalB + r.b.time

        Console.SetOut(TextWriter.Null)

    Console.SetOut(writer)

    printColor ConsoleColor.Blue
    <| sprintf
        "Totals  |                  | %s |                  | %s | %s"
        (formatTimeSpan totalA)
        (formatTimeSpan totalB)
        (formatTimeSpan (totalA + totalB))
