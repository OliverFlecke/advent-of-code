namespace AdventOfCode.Solutions

open AdventOfCode
open AdventOfCode.Utils

[<AutoOpen>]
module private Day23 =
    type Amphipod =
        | A
        | B
        | C
        | D

        static member public energy =
            function
            | A -> 1u
            | B -> 10u
            | C -> 100u
            | D -> 1000u

        static member public target =
            function
            | A -> 3
            | B -> 5
            | C -> 7
            | D -> 9

        static member public distToTarget a pos =
            uint (abs (Amphipod.target a - fst pos))
            + if snd pos = 1 then 1u else 0u

        static member fromString s =
            match s with
            | 'A' -> Some A
            | 'B' -> Some B
            | 'C' -> Some C
            | 'D' -> Some D
            | _ -> None

    type Position = int * int
    type State = Map<Position, Amphipod option>

    let columns = [ 3; 5; 7; 9 ]

    let validPositions =
        seq {
            for x in 1 .. 11 do
                yield (x, 1)

            for y in 2 .. 3 do
                for x in columns do
                    yield (x, y)
        }

    let printState state =
        SparseMatrix.print
            (Option.map (fun x ->
                match x with
                | None -> "."
                | Some a -> string a)
             >> Option.defaultValue " ")
            state

    let parse s : State =
        let lines = splitLines s

        Seq.fold
            (fun state p ->
                Amphipod.fromString lines.[snd p].[fst p]
                |> Option.map (fun x -> Map.add p (Some x) state)
                |> Option.defaultValue (Map.add p None state))
            SparseMatrix.empty
            validPositions

    let isAtGoal state a (x, y) =
        let belowIsAtGoal =
            Map.tryFind (x, y + 1) state
            |> Option.flatten
            |> Option.map ((=) a)
            |> Option.defaultValue false

        Amphipod.target a = x
        && (y = 3 || (y = 2 && belowIsAtGoal))

    let canStop state a (from: Position) (dst: Position) =
        isAtGoal state a dst
        || (snd from <> 1
            && not <| List.contains (fst dst) columns)

    let add (a: Position) (b: Position) = (fst a + fst b), (snd a + snd b)

    let stateCost (state: State) =
        state
        |> Map.toSeq
        |> Seq.filter (snd >> Option.isSome)
        |> Seq.map (fun (p, a) -> p, Option.get a)
        |> Seq.sumBy (fun (p, a) -> Amphipod.distToTarget a p * Amphipod.energy a)

    let solve (state: SparseMatrix<Amphipod option>) =
        let ps = Set.ofSeq validPositions
        let isValidPosition = flip Set.contains ps

        let isPositionFree state =
            flip Map.tryFind state
            >> Option.flatten
            >> Option.isNone

        let isSeen = flip Set.contains

        let isSolved state =
            state
            |> (Map.filter (fun _ a -> Option.isSome a)
                >> Map.map (fun _ a -> Option.get a)
                >> Map.forall (fun pos a -> isAtGoal state a pos))

        let search_for_actions amphipod state start =
            let rec search visited frontier result =
                match PriorityQ.popMin frontier with
                | None -> result
                // If amphipod can move directly to its goal, throw away all other states
                | Some ((cost, pos), _) when isAtGoal state amphipod pos ->
                    // printfn $"Found move to goal for {amphipod} from {start} to {pos}"
                    Map.ofSeq [ (pos, cost) ]
                | Some ((cost, pos), frontier') ->
                    let visited' = Set.add pos visited

                    let result' =
                        if pos <> start && canStop state amphipod start pos then
                            Map.add pos cost result
                        else
                            result

                    let frontier'' =
                        directionWithoutDiagonals
                        |> Seq.map (add pos)
                        |> Seq.filter isValidPosition
                        |> Seq.filter (isPositionFree state)
                        |> Seq.filter (isSeen visited >> not)
                        |> Seq.fold (fun f p -> PriorityQ.push (cost + 1u) p f) frontier'

                    search visited' frontier'' result'

            search Set.empty (PriorityQ.singleton 0u start) Map.empty

        let nextStates state =
            state
            |> Map.filter (fun _ t -> Option.isSome t)
            |> Map.map (fun _ t -> Option.get t)
            |> Map.filter (fun p a -> isAtGoal state a p |> not)
            |> Map.toSeq
            |> Seq.map (fun (pos, a) ->
                let state' = Map.add pos None state

                search_for_actions a state' pos
                |> Map.toSeq
                |> Seq.map (fun (p, c) ->
                    let cost = Amphipod.distToTarget a pos + c
                    (cost, c * Amphipod.energy a), Map.add p (Some a) state'))
            |> Seq.concat

        // state
        // |> nextStates
        // |> Seq.map snd
        // |> Seq.iter printState

        let rec helper visited frontier =
            match PriorityQ.popMin frontier with
            | None -> failwith "No solution"
            | Some ((_, (state, tail, energy)), _) when isSolved state ->
                tail
                |> Seq.rev
                |> Seq.iter (fun (s, e) ->
                    printfn $"Cost: {e}"
                    printState s)

                printfn $"States visited: {Set.count visited}"
                energy, state
            | Some ((cost, (state, tail, energy)), frontier) ->
                let visited' = Set.add state visited
                // if Set.count visited' % 1000 = 0 then
                //     printfn $"Visited {Set.count visited'} states. Current cost {energy}"

                // printf "Cost: %d\n" cost
                // printState state
                // printfn ""

                let frontier' =
                    nextStates state
                    |> Seq.filter (snd >> flip Set.contains visited' >> not)
                    |> Seq.fold
                        (fun f ((c, e), s) ->
                            PriorityQ.push (e + stateCost state) (s, (s, energy + e) :: tail, energy + e) f)
                        frontier

                // frontier'
                // |> PriorityQ.toSeq
                // |> Seq.map snd
                // |> Seq.iter printState
                helper visited' frontier'

        let (cost, state) =
            helper Set.empty (PriorityQ.singleton 0u (state, [], 0u))

        printfn $"Cost {cost}"
        // print state

        cost

type Year2021Day23() =
    interface ISolution with
        member _.year = 2021
        member _.day = 23

        member _.testA =
            seq [ Int 12521,
                  Some(
                      "#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########"
                  ) ]

        member _.testB =
            seq [ Int 44169,
                  Some(
                      "#############
#...........#
###B#C#B#D###
  #D#C#B#A#
  #D#B#A#C#
  #A#D#C#A#
  #########"
                  ) ]

        member _.solveA input = input |> parse |> solve |> int |> Int

        member _.solveB input = input |> parse |> solve |> int |> Int
