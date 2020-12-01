#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Debug/net5.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2020 1
let numbers = ints data

let rec combinationsImpl acc size set =
    seq {
        match size, set with
        | n, x :: xs ->
            if n > 0
            then yield! combinationsImpl (x :: acc) (n - 1) xs
            if n >= 0 then yield! combinationsImpl acc n xs
        | 0, [] -> yield acc
        | _, [] -> ()
    }

let combinations n set = combinationsImpl [] n set

let a =
    numbers
    |> Seq.toList
    |> combinations 2
    |> Seq.filter (fun (x :: y :: _) -> x + y = 2020)
    |> Seq.take 1
    |> Seq.map (fun (x :: y :: _) -> x * y)
    |> Seq.head

submit 2020 1 Level.One a

let b =
    numbers
    |> Seq.toList
    |> combinations 3
    |> Seq.filter (fun (x :: y :: z :: _) -> x + y + z = 2020)
    |> Seq.take 1
    |> Seq.map (fun (x :: y :: z :: _) -> x * y * z)
    |> Seq.head

submit 2020 1 Level.Two b
