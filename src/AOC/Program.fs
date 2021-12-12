printfn "*** AOC solutions ***\n"

open System
open System.Reflection
open AdventOfCode

let solutions =
    Assembly.GetExecutingAssembly().GetTypes()
    |> Seq.filter (fun x ->
        x.GetInterfaces()
        |> Seq.exists (fun i -> i = typeof<ISolution>))
    |> Seq.map (fun t -> Activator.CreateInstance(t) :?> ISolution)

for s in solutions do
    printfn "Running for %i day %i" s.year s.day
    let data = input s.year s.day
    let a = s.solveA data

    submit s.year s.day Level.One a |> ignore