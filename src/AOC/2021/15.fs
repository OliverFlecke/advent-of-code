namespace AdventOfCode.Solutions

open AdventOfCode
open AdventOfCode.Utils

[<AutoOpen>]
module private Day15 =
    let parse =
        splitLines
        >> array2D
        >> Array2D.map (string >> uint)

    module Array2D =
        let dirs = seq [ (0, 1); (1, 0); (-1, 0); (0, -1) ]

        let neighbors matrix pos =
            dirs
            |> Seq.map (fun (x, y) -> x + fst pos, y + snd pos)
            |> Seq.filter (fun (x, y) ->
                0 <= x
                && x < Array2D.length1 matrix
                && 0 <= y
                && y < Array2D.length2 matrix)

    let cost matrix = uncurry <| Array2D.get matrix

    let search matrix =
        let goal =
            Array2D.length1 matrix - 1, Array2D.length2 matrix - 1

        let rec search' (visited: Set<int * int>) frontier =
            match PriorityQ.popMin frontier with
            | None -> []
            | Some ((c, path), rest) ->
                match path with
                | [] -> failwith "Should not be possible"
                | pos :: _ when pos = goal -> path
                | pos :: _ as path ->
                    let visited' =
                        Array2D.neighbors matrix pos
                        |> Seq.fold (flip Set.add) visited

                    let frontier' =
                        (Array2D.neighbors matrix pos)
                        |> Seq.filter (flip Set.contains visited >> not)
                        |> Seq.fold (fun front n -> PriorityQ.push (c + cost matrix n) (n :: path) front) rest

                    search' visited' frontier'

        let start = 0, 0

        search' (set [ start ]) (PriorityQ.push 0u [ start ] PriorityQ.empty)
        |> List.rev

    let pathCost (matrix: uint [,]) (path: seq<int * int>) =
        let start = Seq.head path

        Seq.sumBy (cost matrix) path
        - matrix.[fst start, snd start]

    let scaleGraph scale graph =
        let translate x = modulo (Array2D.length1 graph) x
        let translate y = modulo (Array2D.length2 graph) y

        let initer x y =
            let value =
                int (graph.[translate x, translate y])
                + x / Array2D.length1 graph
                + y / Array2D.length2 graph

            if value > 9 then value - 9 else value

        Array2D.init (scale * Array2D.length1 graph) (scale * Array2D.length2 graph) initer
        |> Array2D.map uint


type Year2021Day15() =
    interface ISolution with
        member _.year = 2021
        member _.day = 15

        member _.testA = seq [ (UInt 40u, None) ]
        member _.testB = seq [ (UInt 315u, None) ]

        member _.solveA input =
            let graph = parse input
            let path = search graph

            UInt <| pathCost graph path

        member _.solveB input =
            let graph = parse input |> scaleGraph 5
            let path = search graph

            UInt <| pathCost graph path
