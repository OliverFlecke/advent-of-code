namespace AdventOfCode.Solutions

open AdventOfCode
open AdventOfCode.Utils

[<AutoOpen>]
module private Day24 =
    let parse = splitLines

    let solve s =
        let extractNumber (s: string) = s[6..] |> int
        let combine = List.toSeq >> Seq.map string >> String.concat "" >> int64

        let sets =
            s
            |> Seq.chunkBySize 18 // Each 18 instructions matches one digit
            |> Seq.collect (fun c -> [c.[5]; c.[15] ])
            |> Seq.map extractNumber
            |> Seq.chunkBySize 2
            |> Seq.map (fun c -> (c.[0], c.[1]))
            |> List.ofSeq

        let ns = seq { 1 .. 9 } |> List.ofSeq

        let rec check (digits: int list) i z =
            if Seq.length digits = List.length sets then
                if z = 0 then
                    Seq.singleton <| combine digits
                else Seq.singleton <| 0
            else
                let sub range f =
                    range |> Seq.collect (fun w -> check (digits @ [w]) (i + 1) (f w))

                if fst sets.[i] < 0 then
                    sub (ns |> Seq.filter (fun n -> n = ((z % 26) + fst sets[i]))) (fun _ -> z / 26)
                else sub ns (fun w -> z * 26 + w + snd sets[i])

        let result = check [] 0 0 |> List.ofSeq
        (Seq.max result, Seq.min result)

type Year2021Day24() =
    interface ISolution with
        member _.year = 2021
        member _.day = 24

        member _.testA = seq []
        member _.testB = seq []

        member _.solveA input =
            input |> parse |> solve |> fst |> Int64

        member _.solveB input =
            input |> parse |> solve |> snd |> Int64
