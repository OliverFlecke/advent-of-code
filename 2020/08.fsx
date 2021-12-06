#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open System
open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2020 8
let testData = testInput 2020 8

let parse = splitLines

let printProgram (inst: string []) = printfn "%s" <| String.Join("; ", inst)

let run (inst: string []) =
    let mutable acc = 0
    let mutable i = 0
    let mutable visited = Set.empty

    while not <| visited.Contains(i) && i < inst.Length do
        visited <- visited.Add(i)
        match inst.[i] with
        | ReMatch "nop" _ -> i <- i + 1
        | ReMatch "jmp (?<number>[+-]\d+)" [ number ] -> i <- i + (int number)
        | ReMatch "acc (?<number>[+-]\d+)" [ number ] ->
            acc <- acc + (int number)
            i <- i + 1
        | x -> failwith $"Unknown instruction {x}"

    (acc, i >= inst.Length)

let solve = parse >> run >> fst

printfn "%d" <| solve testData
submit 2020 8 Level.One <| solve data

// Level 2
let generate (inst: string []) =
    seq {
        for i = inst.Length - 1 downto 0 do
            // printfn $"looking at {i} with {inst.[i]}"
            match inst.[i] with
            | ReMatch "nop (?<rest>.*)" [ rest ] ->
                let copy = Array.copy inst
                copy.[i] <- $"jmp {rest}"
                yield copy
            | ReMatch "jmp (?<rest>.*)" [ rest ] ->
                let copy = Array.copy inst
                copy.[i] <- $"nop {rest}"
                yield copy
            | _ -> ()
    }

let solve2 =
    parse
    >> generate
    >> Seq.map run
    >> Seq.skipWhile (snd >> not)
    >> Seq.head
    >> fst

printfn "%d" <| solve2 testData
submit 2020 8 Level.Two <| solve2 data
