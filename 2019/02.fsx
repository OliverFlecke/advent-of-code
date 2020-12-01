#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Debug/net5.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

type Code = int []

type Op =
    | Add = 1
    | Mul = 2
    | Ter = 99

let data = input 2019 2

let op (f: int -> int -> int) a b d (arr: Code) = arr.[d] <- (f arr.[a] arr.[b])
let add = op ((+))
let mul = op ((*))

let step (arr: Code) (i: int) =
    match arr.[i] with
    | 1 -> add arr.[i + 1] arr.[i + 2] arr.[i + 3] arr
    | 2 -> mul arr.[i + 1] arr.[i + 2] arr.[i + 3] arr
    | _ -> ()

let execute (arr: Code) =
    let mutable i = 0
    while arr.[i] <> int Op.Ter do
        step arr i
        i <- i + 4
    arr.[0]

let pp (code: Code) =
    Seq.iter (fun x -> printf $"{x}, ") code
    printfn $""

let run noun verb =
    let arr =
        data.Trim().Split(",")
        |> Seq.map int
        |> Seq.toArray

    arr.[1] <- noun
    arr.[2] <- verb
    execute arr

let a = run 12 2
submit 2019 2 Level.One a


// Level 2
let expected = 19690720

let b =
    Seq.allPairs [ 0 .. 99 ] [ 0 .. 99]
    |> Seq.skipWhile (fun (n, v) -> run n v <> expected)
    |> Seq.head

submit 2019 2 Level.Two (100 * (fst b) + (snd b))
