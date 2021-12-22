namespace AdventOfCode.Solutions

open AdventOfCode
open AdventOfCode.Utils

[<AutoOpen>]
module private Day22 =
    type Range = { start: int; finish: int }

    type Cube =
        { x: Range
          y: Range
          z: Range }

        member self.values =
            seq {
                for x in max -50 self.x.start .. min 50 self.x.finish do
                    for y in max -50 self.y.start .. min 50 self.y.finish do
                        for z in max -50 self.z.start .. min 50 self.z.finish do
                            yield x, y, z
            }

    type Command =
        | On of Cube
        | Off of Cube



    let parseRange =
        function
        | ReMatch "x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)" [ xa; xb; ya; yb; za; zb ] ->
            { x = { start = int xa; finish = int xb }
              y = { start = int ya; finish = int yb }
              z = { start = int za; finish = int zb } }
        | s -> failwithf "Cannot parse range: %s" s

    let parse =
        splitLines
        >> Seq.map (function
            | ReMatch "on (.*)$" [ range ] -> On <| parseRange range
            | ReMatch "off (.*)$" [ range ] -> Off <| parseRange range
            | s -> failwithf "Cannot parse %s" s)

    let solve commands =
        let ``process`` state =
            function
            | On range -> Seq.fold (flip Set.add) state range.values
            | Off range -> Seq.fold (flip Set.remove) state range.values

        Seq.fold ``process`` Set.empty commands

type Year2021Day22() =
    interface ISolution with
        member _.year = 2021
        member _.day = 22

        member _.testA = seq [ (Int 590784, None) ]
        member _.testB = seq []

        member _.solveA input =
            input |> parse |> solve |> Set.count |> Int

        member _.solveB input = Int 0
