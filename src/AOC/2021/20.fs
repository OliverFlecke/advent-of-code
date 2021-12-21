namespace AdventOfCode.Solutions

open AdventOfCode
open AdventOfCode.Utils

[<AutoOpen>]
module private Day20 =
    let parse (input: string) =
        let split = input.Split("\n\n")

        let algorithm =
            split.[0] |> Seq.map ((=) '#') |> Array.ofSeq

        let image =
            split.[1]
            |> splitLines
            |> Seq.mapi (fun y s -> Seq.mapi (fun x c -> if c = '#' then Some(x, y) else None) s)
            |> Seq.collect id
            |> filterOption
            |> Seq.fold (flip Set.add) Set.empty

        algorithm, image

    type Frame =
        { infiniteOn: bool
          minX: int
          maxX: int
          minY: int
          maxY: int }

    let enhance frame (algorithm: bool array) image =
        let outOfBounds (x, y) =
            x < frame.minX
            || x > frame.maxX
            || y < frame.minY
            || y > frame.maxY

        let isOn pos =
            (frame.infiniteOn && outOfBounds pos)
            || Set.contains pos image

        let index (x, y) =
            Seq.allPairs { y - 1 .. y + 1 } { x - 1 .. x + 1 }
            |> Seq.fold (fun acc (y, x) -> (acc <<< 1) + (if isOn (x, y) then 1 else 0)) 0

        let output =
            Seq.allPairs { frame.minX - 1 .. frame.maxX + 1 } { frame.minY - 1 .. frame.maxY + 1 }
            |> Seq.filter (fun pos -> algorithm.[index pos])
            |> Set.ofSeq

        let frame' =
            { infiniteOn = algorithm.[index (frame.minX - 2, frame.maxY - 2)]
              minX = frame.minX - 1
              maxX = frame.maxX + 1
              minY = frame.minY - 1
              maxY = frame.maxY + 1 }

        output, frame'

    let multiEnhance (algorithm: bool array) image =
        let frame =
            { infiniteOn = false
              minX = 0
              maxX = image |> Seq.map fst |> Seq.max
              minY = 0
              maxY = image |> Seq.map snd |> Seq.max }

        Seq.unfold (fun (img, frame) -> Some(img, enhance frame algorithm img)) (image, frame)

type Year2021Day20() =
    interface ISolution with
        member _.year = 2021
        member _.day = 20

        member _.testA = seq [ (Int 35, None) ]
        member _.testB = seq [ (Int 3351, None) ]

        member _.solveA input =
            let algorithm, image = parse input

            multiEnhance algorithm image
            |> Seq.item 2
            |> Set.count
            |> Int

        member _.solveB input =
            let algorithm, image = parse input

            multiEnhance algorithm image
            |> Seq.item 50
            |> Set.count
            |> Int
