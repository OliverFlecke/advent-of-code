#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net5.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2020 13
let testData = testInput 2020 13

let parse =
    splitLines
    >> List.ofSeq
    >> (function
    | [ arrive; busses ] ->
        (int arrive,
         busses.Split(",")
         |> Seq.filter ((<>) "x")
         |> Seq.map int)
    | _ -> failwith "Unable to parse input")

let busLeaves minute = Seq.exists (fun b -> minute % b = 0)

let run (start, busses) =
    let mutable minute = start
    while not <| busLeaves minute busses do
        minute <- minute + 1

    let bus =
        Seq.filter (fun b -> minute % b = 0) busses
        |> Seq.head

    (minute - start) * bus

let solve = parse >> run

solve testData |> print

submit 2020 13 Level.One <| solve data

// Level 2
let parse2 =
    splitLines
    >> List.ofSeq
    >> (function
    | [ _; busses ] ->
        busses.Split(",")
        |> Seq.map (function
            | "x" -> None
            | n -> Some <| int n)
    | _ -> failwith "unable to parse input")

let rec gcd a b = if b = 0L then abs a else gcd b (a % b)

let modularInverse n g =
    let rec fN n i g e l a =
        match e with
        | 0L -> g
        | _ ->
            let o = n / e
            fN e l a (n - o * e) (i - o * l) (g - o * a)

    (n + (fN n 1L 0L g 0L 1L)) % n

let crt n g =
    match Seq.fold (fun n g -> if (gcd n g) = 1L then n * g else 0L) 1L g with
    | 0L -> None
    | fN ->
        Some
            ((Seq.fold2 (fun n i g ->
                n
                + i
                * (fN / g)
                * (modularInverse g ((fN / g) % g))) 0L n g) % fN)

let time i b =
    match b with
    | Some b -> Some((b - i) % b, b)
    | None -> None

let solve2 =
    parse2
    >> Seq.mapi time
    >> filterOption
    >> List.ofSeq
    >> List.unzip
    >> fun (ts, bs) -> crt (Seq.ofList ts |> Seq.map int64) (Seq.ofList bs |> Seq.map int64)
    >> Option.get

[ testData
  "\n17,x,13,19"
  "\n67,7,59,61"
  "\n67,x,7,59,61"
  "\n67,7,x,59,61"
  "\n1789,37,47,1889" ]
|> Seq.map solve2
|> Seq.iter print

submit 2020 13 Level.Two <| solve2 data
