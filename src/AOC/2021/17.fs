namespace AdventOfCode.Solutions

open AdventOfCode
open AdventOfCode.Utils

[<AutoOpen>]
module private Day17 =
    let parse =
        function
        | ReMatch "target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)" [ xMin; xMax; yMin; yMax ] ->
            (int xMin, int xMax), (int yMin, int yMax)
        | _ -> failwith "Cannot parse input"

    let hitsTarget ((xMin, xMax), (yMin, yMax)) (x, y) =
        xMin <= x && x <= xMax && yMin <= y && y <= yMax

    let simulate target velocity =
        let (xMin, xMax), (yMin, yMax) = target

        let rec step (x, y) (vx, vy) path =
            if hitsTarget target (x, y) then
                Some path
            elif xMax < x || y < yMin || (vx = 0 && x < xMin) then
                None
            else
                let pos' = (x + vx, y + vy)
                step pos' (max 0 <| vx - 1, vy - 1) (pos' :: path)

        step (0, 0) velocity []

    let solve target =
        seq {
            for x in 1 .. 500 do
                for y in -500 .. 500 do
                    match simulate target (x, y) with
                    | Some path ->
                        // printfn "Intial %A. Path %A" (x, y) (path |> Seq.map snd |> Seq.max)
                        path |> Seq.map snd |> Seq.max
                    | None -> ()
        }



type Year2021Day17() =
    interface ISolution with
        member _.year = 2021
        member _.day = 17

        member _.testA = seq [ (Int 45, None) ]
        member _.testB = seq []

        member _.solveA input =
            let target = parse input
            print target

            solve target |> Seq.max |> Int

        member _.solveB input = Int 0
