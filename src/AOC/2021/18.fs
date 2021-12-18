namespace AdventOfCode.Solutions

open AdventOfCode
open AdventOfCode.Utils

[<AutoOpen>]
module private Day18 =
    type Node =
        | Leaf of int
        | Pair of Node * Node

    let rec parseLine str =
        match str with
        | ReMatch "^(\d+)" [ value ] -> Leaf <| int value, str.[value.Length..]
        | _ when Seq.head str = '[' ->
            let left, rest' = parseLine str.[1..]
            let right, str' = parseLine rest'
            Pair(left, right), str'
        | _ when Seq.head str = ',' -> parseLine str.[1..]
        | _ when Seq.head str = ']' -> parseLine str.[1..]
        | _ -> failwithf "Unmatched %s" str

    let parse = splitLines >> Seq.map (parseLine >> fst)

    let rec addLeft a =
        function
        | Leaf b -> Leaf <| a + b
        | Pair (b, c) -> Pair(addLeft a b, c)

    let rec addRight node a =
        match node with
        | Leaf b -> Leaf <| a + b
        | Pair (b, c) -> Pair(b, addRight c a)

    type Explosion =
        | NoExplosion
        | Add of int * int
        | CarryLeft of int
        | CarryRight of int
        | Exploded

    let rec explode depth expr =
        match depth, expr with
        | _, Leaf v -> (NoExplosion, Leaf v)
        | 4, Pair (Leaf a, Leaf b) -> Add(a, b), Leaf 0
        | 4, Pair (a, b) -> failwithf "Unhandled %A" expr
        | _, Pair (a, b) ->
            match explode (depth + 1) a with
            | Add (l, r), s -> CarryLeft l, Pair(s, addLeft r b)
            | CarryLeft l, s -> CarryLeft l, Pair(s, b)
            | CarryRight r, s -> Exploded, Pair(s, addLeft r b)
            | Exploded, s -> Exploded, Pair(s, b)
            | NoExplosion, _ ->
                match explode (depth + 1) b with
                | Add (l, r), s -> CarryRight r, Pair(addRight a l, s)
                | CarryLeft l, s -> Exploded, Pair(addRight a l, s)
                | CarryRight r, s -> CarryRight r, Pair(a, s)
                | Exploded, s -> Exploded, Pair(a, s)
                | NoExplosion, _ -> NoExplosion, Pair(a, b)

    type Split =
        | NoSplit
        | Split of Node

    let rec split =
        function
        | Leaf x when x >= 10 -> Split <| Pair(Leaf <| x / 2, Leaf <| (x + 1) / 2)
        | Leaf _ -> NoSplit
        | Pair (a, b) ->
            match split a with
            | Split s -> Split <| Pair(s, b)
            | NoSplit ->
                match split b with
                | Split s -> Split <| Pair(a, s)
                | NoSplit -> NoSplit

    let rec reduce expr =
        match explode 0 expr with
        | Add (l, r), _ -> failwith "Cannot add at top level"
        | CarryLeft _, e
        | CarryRight _, e
        | Exploded, e -> reduce e
        | NoExplosion, _ ->
            match split expr with
            | Split e -> reduce e
            | NoSplit -> expr


    let add a b = Pair(a, b) |> reduce

    let rec magnitude =
        function
        | Leaf v -> v
        | Pair (a, b) -> 3 * magnitude a + 2 * magnitude b

type Year2021Day18() =
    interface ISolution with
        member _.year = 2021
        member _.day = 18

        member _.testA =
            seq [ (Int 143, Some "[1,2]\n[[3,4],5]")
                  (Int 143, Some "[[1,2],[[3,4],5]]")
                  (Int 1384, Some "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")
                  (Int 445, Some "[[[[1,1],[2,2]],[3,3]],[4,4]]")
                  (Int 791, Some "[[[[3,0],[5,3]],[4,4]],[5,5]]")
                  (Int 1137, Some "[[[[5,0],[7,4]],[5,5]],[6,6]]")
                  (Int 3488, Some "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")
                  (Int 4140, None) ]

        member _.testB = seq [ (Int 3993, None) ]

        member _.solveA input =
            input
            |> parse
            |> Seq.reduce add
            |> magnitude
            |> Int

        member _.solveB input =
            input
            |> parse
            |> Seq.pairs
            |> Seq.map (uncurry add >> magnitude)
            |> Seq.max
            |> Int
