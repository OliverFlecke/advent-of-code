#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Debug/net5.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils

let data = input 2015 2

let surfaceArea (l, w, h) = (2 * l * w) + (2 * w * h) + (2 * h * l)

let twoSmallest (l, w, h) = [ l; w; h ] |> (Seq.sort >> Seq.take 2)
let extra = twoSmallest >> Seq.reduce (*)

let paper dim = surfaceArea dim + extra dim

let parse (str: string) =
    match str.Split("x") with
    | [| l; w; h |] -> (int l, int w, int h)
    | _ -> failwith "Unable to parse data"

let solve = splitLines >> Seq.sumBy (parse >> paper)

submit 2015 2 Level.One <| solve data

// Level 2
let bow (l, w, h) = l * w * h

let ribbonMain dim =
    match twoSmallest dim |> Seq.toList with
    | [ a; b ] -> a + a + b + b
    | _ -> failwith "unable to find smallest dimensions"

let ribbon dim = bow dim + ribbonMain dim

let solve2 =
    splitLines >> Seq.sumBy (parse >> ribbon)

submit 2015 2 Level.Two <| solve2 data
