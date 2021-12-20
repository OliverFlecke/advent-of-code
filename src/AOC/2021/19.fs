namespace AdventOfCode.Solutions

open AdventOfCode
open AdventOfCode.Utils

[<AutoOpen>]
module private Day19 =
    type Vector3D = int * int * int

    let parseSensor (str: string) =
        let lines = str |> splitLines

        let scannerId =
            match Seq.head lines with
            | ReMatch "scanner (\d+)" [ id ] -> int id
            | _ -> failwith "Unable to get scanner Id"

        let beacons =
            Seq.tail lines
            |> Seq.map (fun line ->
                match line.Split(",") with
                | [| x; y; z |] -> int x, int y, int z
                | _ -> failwithf "Unable to parse line '%s'" line)
            |> Set.ofSeq

        scannerId, beacons

    let parse (input: string) =
        input.Split("\n\n")
        |> Seq.map parseSensor
        |> Seq.toList

    let fst3 (x, _, _) = x
    let snd3 (_, y, _) = y
    let trd3 (_, _, z) = z

    let operate op (x0, y0, z0) (x1, y1, z1) = op x0 x1, op y0 y1, op z0 z1
    let add = operate (+)
    let sub = operate (-)
    let mul = operate (*)

    type Axis =
        | XPos
        | XNeg
        | YPos
        | YNeg
        | ZPos
        | ZNeg

    type Rotation =
        | Rot0
        | Rot90
        | Rot180
        | Rot270

    let transform up rotation (x, y, z) =
        let (ox, oy, oz) =
            match up with
            | YPos -> x, y, z
            | YNeg -> x, -y, -z
            | XPos -> y, x, -z
            | XNeg -> y, -x, z
            | ZPos -> y, z, x
            | ZNeg -> y, -z, -x

        match rotation with
        | Rot0 -> ox, oy, oz
        | Rot90 -> oz, oy, -ox
        | Rot180 -> -ox, oy, -oz
        | Rot270 -> -oz, oy, ox

    let rotations = [ Rot0; Rot90; Rot180; Rot270 ]
    let axes = [ XPos; XNeg; YPos; YNeg; ZPos; ZNeg ]
    let axesRotations = List.allPairs axes rotations

    let align s1 s2 =
        axesRotations
        |> Seq.tryPick (fun (axis, rot) ->
            let rotatedS2 = Seq.map (transform axis rot) s2

            Seq.allPairs s1 rotatedS2
            |> Seq.tryPick (fun (b1, b2) ->
                let delta = sub b1 b2
                let alignedS2 = Seq.map (add delta) rotatedS2

                let intersection =
                    Seq.filter (fun b -> Set.contains b s1) alignedS2

                if (Seq.truncate 12 intersection |> Seq.length) = 12 then
                    Some(Set.ofSeq alignedS2, delta, axis, rot)
                else
                    None))

    let rec reduce (scannersBeacons: (Set<Vector3D> * Set<Vector3D>) list) =
        let rec merge (s1, b1) (accScanners, accBeacons) rem unmerged =
            match rem with
            | [] -> accScanners, accBeacons, unmerged
            | (s2, b2) :: rest ->
                match align b1 b2 with
                | None -> merge (s1, b1) (accScanners, accBeacons) rest ((s2, b2) :: unmerged)
                | Some (alignedBeacons, delta, axis, rot) ->
                    let alignedScanners =
                        Set.map (transform axis rot >> add delta) s2

                    let newScanners = Set.union alignedScanners accScanners
                    let newBeacons = Set.union alignedBeacons accBeacons
                    merge (s1, b1) (newScanners, newBeacons) rest unmerged

        let scanners, beacons, unmerged =
            merge (List.head scannersBeacons) (List.head scannersBeacons) (List.tail scannersBeacons) []

        match unmerged with
        | [] -> scanners, beacons
        | _ ->
            printfn "Remaining to be merged: %d" (List.length unmerged)
            reduce ((scanners, beacons) :: unmerged)

    let solve =
        parse
        >> List.map snd
        >> List.map (fun scans -> Set.singleton (0, 0, 0), scans)
        >> reduce


type Year2021Day19() =
    interface ISolution with
        member _.year = 2021
        member _.day = 19

        member _.testA = seq [ (Int 79, None) ]
        member _.testB = seq [ (Int 3621, None) ]

        member _.solveA input =
            input |> solve |> snd |> Set.count |> Int

        member _.solveB input =
            let scanners, _ = solve input

            Seq.allPairs scanners scanners
            |> Seq.filter (uncurry (<>))
            |> Seq.map (uncurry sub)
            |> Seq.map (fun (x, y, z) -> abs x + abs y + abs z)
            |> Seq.max
            |> Int
